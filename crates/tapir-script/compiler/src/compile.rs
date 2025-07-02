use std::{collections::HashMap, ops::ControlFlow, path::Path};

use optimisations::UnusedFunction;
use symtab_visitor::{SymTab, SymTabVisitor};
use type_visitor::{TriggerId, TypeTable, TypeVisitor};

use crate::{
    ast::{self, BinaryOperator, Function, FunctionId, MaybeResolved, Statement, SymbolId},
    grammar,
    lexer::Lexer,
    reporting::Diagnostics,
    tokens::FileId,
    types::Type,
    EventHandler, EventHandlerArgument, Trigger,
};

mod ir;
mod loop_visitor;
mod optimisations;
mod symtab_visitor;
mod type_visitor;

use opcodes::{MathsOp, Opcode};

#[derive(Clone, Debug)]
pub struct Property {
    pub ty: Type,
    pub index: usize,
    pub name: String,
}

pub struct CompileSettings {
    pub properties: Vec<Property>,
    pub enable_optimisations: bool,
}

impl CompileSettings {
    pub(crate) fn is_property(&self, symbol_id: SymbolId) -> bool {
        symbol_id.0 < self.properties.len()
    }

    pub(crate) fn property_symbols(&self) -> impl Iterator<Item = SymbolId> {
        (0..self.properties.len()).map(SymbolId)
    }
}

pub fn compile(
    filename: impl AsRef<Path>,
    input: &str,
    settings: &CompileSettings,
) -> Result<Bytecode, Diagnostics> {
    let file_id = FileId::new(0);

    let mut diagnostics = Diagnostics::new(file_id, filename, input);

    let lexer = Lexer::new(input, file_id);
    let parser = grammar::ScriptParser::new();

    let mut ast = match parser.parse(file_id, &mut diagnostics, lexer) {
        Ok(ast) => ast,
        Err(e) => {
            diagnostics.add_lalrpop(e, file_id);
            return Err(diagnostics);
        }
    };

    let mut sym_tab_visitor = SymTabVisitor::new(settings, &mut ast.functions, &mut diagnostics);
    let mut type_visitor = TypeVisitor::new(settings, &ast.functions);

    for function in &mut ast.functions {
        sym_tab_visitor.visit_function(function, &mut diagnostics);
        loop_visitor::visit_loop_check(function, &mut diagnostics);

        type_visitor.visit_function(function, sym_tab_visitor.get_symtab(), &mut diagnostics);
    }

    optimisations::optimise(&mut ast.functions, settings, &mut diagnostics);

    let type_table = type_visitor.into_type_table(sym_tab_visitor.get_symtab(), &mut diagnostics);

    if diagnostics.has_any() {
        return Err(diagnostics);
    }

    let mut compiler = Compiler::new(type_table);

    for function in ast.functions {
        if function.meta.has::<UnusedFunction>() {
            continue; // don't need to compile unused functions
        }

        compiler.compile_function(&function, sym_tab_visitor.get_symtab());
    }

    compiler.finalise();

    Ok(compiler.bytecode)
}

struct Compiler<'input> {
    stack: Vec<Option<SymbolId>>,
    loops: Vec<LoopCompliationState>,

    function_calls: Vec<(FunctionId, Jump)>,
    function_locations: HashMap<FunctionId, Label>,

    type_table: TypeTable<'input>,
    bytecode: Bytecode,
}

struct LoopCompliationState {
    loop_start: Label,
    forward_jumps: Vec<Jump>,

    stack_length: usize,
}

impl<'input> Compiler<'input> {
    pub fn new(type_table: TypeTable<'input>) -> Self {
        Self {
            stack: vec![],
            loops: vec![],

            function_calls: vec![],
            function_locations: HashMap::from([(FunctionId(0), Label(0))]),
            bytecode: Bytecode::new(type_table.triggers()),
            type_table,
        }
    }

    pub fn compile_function(&mut self, function: &Function<'input>, symtab: &SymTab) {
        let function_id = *function.meta.get::<FunctionId>().unwrap();
        if function_id != FunctionId(0) {
            // the stack will be arguments, then the return pointer. However, if we're at toplevel, then
            // the stack will be empty to start with
            for argument in &function.arguments {
                let MaybeResolved::Resolved(symbol_id) = argument.name else {
                    panic!("Should have been resolved by the symbol visitor");
                };

                self.stack.push(Some(symbol_id));
            }

            self.stack.push(None);
        }

        self.function_locations
            .insert(function_id, self.bytecode.new_label());

        if function.modifiers.is_event_handler.is_some() {
            self.bytecode.event_handlers.push(EventHandler {
                name: function.name.to_owned(),
                bytecode_offset: self.bytecode.length,
                arguments: function
                    .arguments
                    .iter()
                    .map(|arg| {
                        let MaybeResolved::Resolved(symbol_id) = arg.name else {
                            panic!("Should have been resolved by the symbol visitor");
                        };

                        let symbol_name = symtab.name_for_symbol(symbol_id);
                        EventHandlerArgument {
                            name: symbol_name.to_string(),
                            ty: arg.t.t,
                        }
                    })
                    .collect(),
            });
        }

        self.compile_block(
            &function.statements,
            symtab,
            self.stack.len(),
            function.arguments.len(),
        );

        // if there is no return value, then no return is required to be compiled so we should add one
        if function.return_types.types.is_empty() {
            self.bytecode.add_opcode(Opcode::Return {
                args: function.arguments.len() as u8,
                rets: 0,
                shift: 0, // can be zero because we will have dropped after the block was compiled
            });
        }
    }

    fn compile_block(
        &mut self,
        statements: &[Statement<'input>],
        symtab: &SymTab,
        stack_bottom: usize,
        num_args: usize,
    ) {
        let previous_stack_size = self.stack.len();

        for statement in statements {
            if self.compile_statement(statement, symtab, stack_bottom, num_args)
                == ControlFlow::Break(())
            {
                break;
            };
        }

        self.compile_drop_to(previous_stack_size);
    }

    fn compile_statement(
        &mut self,
        statement: &Statement<'input>,
        symtab: &SymTab,
        stack_bottom: usize,
        num_args: usize,
    ) -> ControlFlow<()> {
        match &statement.kind {
            ast::StatementKind::Error => panic!("Should never have to compile an error"),
            ast::StatementKind::VariableDeclaration { value, .. } => {
                let ident: &SymbolId = statement.meta.get().expect("Should've resolved variable");

                self.compile_expression(value, symtab);
                self.stack.pop();
                self.stack.push(Some(*ident)); // this is now on the stack at this location
            }
            ast::StatementKind::Assignment { value, .. } => {
                let ident: &SymbolId = statement.meta.get().expect("Should've resolved variable");

                self.compile_expression(value, symtab);

                if let Some(property) = symtab.get_property(*ident) {
                    self.bytecode
                        .add_opcode(Opcode::SetProp(property.index as u8));
                    self.stack.pop();
                } else {
                    let offset = self.get_offset(*ident);
                    self.bytecode.add_opcode(Opcode::Move(offset as u8 - 1));
                    self.stack.pop();
                }
            }
            ast::StatementKind::Wait => {
                self.bytecode.add_opcode(Opcode::Wait);
            }
            ast::StatementKind::Nop => {}
            ast::StatementKind::If {
                condition,
                true_block,
                false_block,
            } => {
                let stack_depth_before_if = self.stack.len();

                self.compile_expression(condition, symtab);
                let if_false_jump = self.bytecode.new_jump_if_false();

                self.compile_block(true_block, symtab, stack_bottom, num_args);
                self.compile_drop_to(stack_depth_before_if);

                let if_true_jump = self.bytecode.new_jump();

                let false_target = self.bytecode.new_label();
                self.bytecode.patch_jump(if_false_jump, false_target);

                // insert a fake additional stack value for the bool that will be left
                self.stack.push(None);

                self.compile_block(false_block, symtab, stack_bottom, num_args);

                self.compile_drop_to(stack_depth_before_if);
                let end_target = self.bytecode.new_label();
                self.bytecode.patch_jump(if_true_jump, end_target);
            }
            ast::StatementKind::Block { block } => {
                let stack_depth_before_block = self.stack.len();
                self.compile_block(block, symtab, stack_bottom, num_args);
                self.compile_drop_to(stack_depth_before_block);
            }
            ast::StatementKind::Return { values } => {
                for ret_value in values {
                    self.compile_expression(ret_value, symtab);
                }

                let distance_to_bottom = self.stack.len() - stack_bottom;

                self.bytecode.add_opcode(Opcode::Return {
                    args: num_args as u8,
                    rets: values.len() as u8,
                    shift: distance_to_bottom.try_into().expect("Too far to shift"),
                });

                // we should stop compiling this block
                return ControlFlow::Break(());
            }
            ast::StatementKind::Call { arguments, .. } => {
                let function_id: FunctionId = *statement.meta.get().unwrap();
                let number_of_returns = self.type_table.num_function_returns(function_id);
                let stack_before_call = self.stack.len();

                for argument in arguments {
                    self.compile_expression(argument, symtab);
                }

                let call_jump = self.bytecode.new_call();
                self.function_calls.push((function_id, call_jump));

                // fixup the stack after the call instruction
                self.stack
                    .resize(self.stack.len() - arguments.len() + number_of_returns, None);

                self.compile_drop_to(stack_before_call);
            }
            ast::StatementKind::Spawn { arguments, .. } => {
                let function_id: FunctionId = *statement.meta.get().unwrap();

                for argument in arguments {
                    self.compile_expression(argument, symtab);
                }

                let spawn_jump = self.bytecode.new_spawn(arguments.len() as u8);
                self.function_calls.push((function_id, spawn_jump));

                self.stack.truncate(self.stack.len() - arguments.len());
            }
            ast::StatementKind::Continue => {
                let loop_state = self
                    .loops
                    .last()
                    .expect("Should've worked out that there wasn't a loop for this break yet");
                let stack_size_to_return_to = loop_state.stack_length;
                let loop_start = loop_state.loop_start;

                self.bytecode.add_opcode(Opcode::Drop(
                    (self.stack.len() - stack_size_to_return_to) as u8,
                ));

                let jump = self.bytecode.new_jump();
                self.bytecode.patch_jump(jump, loop_start);
            }
            ast::StatementKind::Break => {
                let loop_state = self
                    .loops
                    .last()
                    .expect("Should've worked out that there wasn't a loop for this break yet");
                let stack_size_to_return_to = loop_state.stack_length;

                self.bytecode.add_opcode(Opcode::Drop(
                    (self.stack.len() - stack_size_to_return_to) as u8,
                ));

                let jump = self.bytecode.new_jump();
                let loop_state = self.loops.last_mut().unwrap();
                loop_state.forward_jumps.push(jump);

                // we should stop compiling this block
                return ControlFlow::Break(());
            }
            ast::StatementKind::Loop { block } => {
                let loop_start = self.bytecode.new_label();
                self.loops.push(LoopCompliationState {
                    loop_start,
                    forward_jumps: vec![],
                    stack_length: self.stack.len(),
                });

                self.compile_block(block, symtab, stack_bottom, num_args);

                let jump = self.bytecode.new_jump();
                self.bytecode.patch_jump(jump, loop_start);

                let loop_end = self.bytecode.new_label();
                let loop_compilation_state = self.loops.pop().expect("There should be a loop here");

                for forward_jump in loop_compilation_state.forward_jumps {
                    self.bytecode.patch_jump(forward_jump, loop_end);
                }
            }
            ast::StatementKind::Trigger { arguments, .. } => {
                let stack_before_trigger = self.stack.len();
                for arg in arguments {
                    self.compile_expression(arg, symtab);
                }

                let &TriggerId(trigger_index) =
                    statement.meta.get().expect("Should have a trigger id");

                self.bytecode
                    .add_opcode(Opcode::Trigger(trigger_index as u8));

                self.stack.truncate(stack_before_trigger);
            }
        }

        ControlFlow::Continue(())
    }

    fn compile_expression(&mut self, value: &ast::Expression<'input>, symtab: &SymTab) {
        match &value.kind {
            ast::ExpressionKind::Integer(i) => {
                if let Ok(as_i8) = i8::try_from(*i) {
                    self.bytecode.add_opcode(Opcode::Push8(as_i8));
                    self.stack.push(None);
                } else {
                    self.bytecode.add_opcode(Opcode::Push32(*i));
                    self.stack.push(None);
                }
            }
            ast::ExpressionKind::Fix(fix) => {
                let raw = fix.to_raw();
                if let Ok(as_i8) = i8::try_from(raw) {
                    self.bytecode.add_opcode(Opcode::Push8(as_i8));
                    self.stack.push(None);
                } else {
                    self.bytecode.add_opcode(Opcode::Push32(raw));
                    self.stack.push(None);
                }
            }
            ast::ExpressionKind::Bool(value) => {
                self.bytecode
                    .add_opcode(Opcode::Push8(if *value { 1 } else { 0 }));
                self.stack.push(None);
            }
            ast::ExpressionKind::Variable(_) => {
                let symbol_id: &SymbolId = value
                    .meta
                    .get()
                    .expect("Should have a symbol id from symbol resolution");

                if let Some(property) = symtab.get_property(*symbol_id) {
                    self.bytecode
                        .add_opcode(Opcode::GetProp(property.index as u8));
                } else {
                    let offset = self.get_offset(*symbol_id);
                    self.bytecode.add_opcode(Opcode::Dup(offset as u8 - 1));
                }

                // should not push the symbol ID because this is a temporary copy
                self.stack.push(None);
            }
            ast::ExpressionKind::BinaryOperation {
                lhs,
                operator: BinaryOperator::Then,
                rhs,
            } => {
                let stack_before = self.stack.len();
                self.compile_expression(lhs, symtab);
                self.compile_drop_to(stack_before);
                self.compile_expression(rhs, symtab);
            }
            ast::ExpressionKind::BinaryOperation { lhs, operator, rhs } => {
                self.compile_expression(lhs, symtab);
                self.compile_expression(rhs, symtab);
                self.stack.pop();
                self.stack.pop();
                self.stack.push(None);

                self.bytecode
                    .add_opcode(Opcode::MathsOp(MathsOp::from(*operator)));
            }
            ast::ExpressionKind::Error => panic!("Should never have to compile an error"),
            ast::ExpressionKind::Nop => panic!("NOP expression will cause stack issues"),
            ast::ExpressionKind::Call { arguments, .. } => {
                let function_id: FunctionId = *value.meta.get().unwrap();

                let number_of_returns = 1;
                for argument in arguments {
                    self.compile_expression(argument, symtab);
                }

                let call_jump = self.bytecode.new_call();
                self.function_calls.push((function_id, call_jump));

                // fixup the stack after the call instruction
                self.stack
                    .resize(self.stack.len() - arguments.len() + number_of_returns, None);
            }
        }
    }

    fn compile_drop_to(&mut self, desired_stack_size: usize) {
        match self.stack.len().cmp(&desired_stack_size) {
            std::cmp::Ordering::Less => {
                panic!("Trying to drop the stack to a size bigger than the current")
            }
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Greater => {
                self.bytecode
                    .add_opcode(Opcode::Drop((self.stack.len() - desired_stack_size) as u8));
                self.stack.truncate(desired_stack_size);
            }
        }
    }

    fn get_offset(&self, symbol_id: SymbolId) -> usize {
        self.stack.len()
            - self
                .stack
                .iter()
                .rposition(|stack_sym| stack_sym == &Some(symbol_id))
                .unwrap()
    }

    fn finalise(&mut self) {
        for (fname, jump) in &self.function_calls {
            let label = self.function_locations[fname];
            self.bytecode.patch_jump(*jump, label);
        }
    }
}

pub mod opcodes {
    use std::fmt::Display;

    use serde::Serialize;

    use crate::ast::BinaryOperator;

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
    pub enum Opcode {
        Push8(i8),
        Push32(i32),
        Dup(u8),
        Drop(u8),
        GetProp(u8),
        SetProp(u8),
        Wait,
        Move(u8),
        MathsOp(MathsOp),
        JumpIfFalse(u16),
        Jump(u16),
        Call(u16),
        Spawn { args: u8, target: u16 },
        Return { args: u8, rets: u8, shift: u8 },
        Trigger(u8),
    }

    impl Display for Opcode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Opcode::Push8(v) => write!(f, "push8\t{v}"),
                Opcode::Push32(v) => write!(f, "push32\t{v}"),
                Opcode::Dup(v) => write!(f, "dup\t{v}"),
                Opcode::Drop(v) => write!(f, "drop\t{v}"),
                Opcode::GetProp(i) => write!(f, "getprop\t{i}"),
                Opcode::SetProp(i) => write!(f, "setprop\t{i}"),
                Opcode::Wait => write!(f, "wait"),
                Opcode::Move(i) => write!(f, "move\t{i}"),
                Opcode::MathsOp(maths_op) => write!(
                    f,
                    "{}",
                    match maths_op {
                        MathsOp::Add => "add",
                        MathsOp::Sub => "sub",
                        MathsOp::Mul => "mul",
                        MathsOp::RealMod => "realmod",
                        MathsOp::RealDiv => "realdiv",
                        MathsOp::EqEq => "==",
                        MathsOp::NeEq => "!=",
                        MathsOp::Gt => ">",
                        MathsOp::GtEq => ">=",
                        MathsOp::Lt => "<",
                        MathsOp::LtEq => "<=",
                        MathsOp::FixMul => "fmul",
                        MathsOp::FixDiv => "fdiv",
                    }
                ),
                Opcode::JumpIfFalse(target) => write!(f, "jif\t{target}"),
                Opcode::Jump(target) => write!(f, "j\t{target}"),
                Opcode::Call(target) => write!(f, "call\t{target}"),
                Opcode::Return { args, rets, shift } => {
                    write!(f, "ret\targs={args} rets={rets} shift={shift}")
                }
                Opcode::Spawn { args, target } => write!(f, "spawn\t{args} {target}"),
                Opcode::Trigger(index) => write!(f, "trigger\t{index}"),
            }
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
    pub enum MathsOp {
        Add,
        Sub,
        Mul,
        RealMod,
        RealDiv,

        EqEq,
        NeEq,
        Gt,
        GtEq,
        Lt,
        LtEq,
        FixMul,
        FixDiv,
    }

    impl From<MathsOp> for bytecode::MathsOp {
        fn from(value: MathsOp) -> Self {
            macro_rules! arm {
                ($($kind:ident),*) => {
                    match value {
                        $(
                            MathsOp::$kind => bytecode::MathsOp::$kind,
                        )*
                    }
                };
            }

            arm!(Add, Sub, Mul, RealMod, RealDiv, EqEq, NeEq, Gt, GtEq, Lt, LtEq, FixMul, FixDiv)
        }
    }

    impl Opcode {
        pub fn size(self) -> usize {
            match self {
                Self::JumpIfFalse(_)
                | Self::Jump(_)
                | Self::Call(_)
                | Self::Return { .. }
                | Self::Spawn { .. } => 2,
                Self::Push32(_) => 3,
                _ => 1,
            }
        }
    }

    impl From<BinaryOperator> for MathsOp {
        fn from(value: BinaryOperator) -> Self {
            use MathsOp::*;

            match value {
                BinaryOperator::Add => Add,
                BinaryOperator::Sub => Sub,
                BinaryOperator::Mul => Mul,
                BinaryOperator::Div => panic!("Shouldn't be compiling div binops"),
                BinaryOperator::Mod => panic!("Shouldn't be compiling mod binops"),
                BinaryOperator::RealDiv => RealDiv,
                BinaryOperator::RealMod => RealMod,
                BinaryOperator::EqEq => EqEq,
                BinaryOperator::NeEq => NeEq,
                BinaryOperator::Gt => Gt,
                BinaryOperator::GtEq => GtEq,
                BinaryOperator::Lt => Lt,
                BinaryOperator::LtEq => LtEq,
                BinaryOperator::FixMul => FixMul,
                BinaryOperator::FixDiv => FixDiv,
                BinaryOperator::Then => panic!("Shouldn't be compiling then binops"),
            }
        }
    }
}

pub struct Bytecode {
    data: Vec<Opcode>,
    /// opcodes have different sizes, this is the current size of the _compiled_ code
    length: usize,

    pub event_handlers: Vec<EventHandler>,
    pub triggers: Vec<Trigger>,
}

impl Bytecode {
    pub fn new(triggers: Vec<Trigger>) -> Self {
        Self {
            data: vec![],
            length: 0,
            event_handlers: vec![],
            triggers,
        }
    }

    pub fn add_opcode(&mut self, opcode: Opcode) {
        self.data.push(opcode);
        self.length += opcode.size();
    }

    fn new_label(&mut self) -> Label {
        Label(
            self.length
                .try_into()
                .expect("Offset bigger than 16 bit maximum"),
        )
    }

    fn new_jump(&mut self) -> Jump {
        self.add_opcode(Opcode::Jump(0));
        Jump(self.data.len() - 1)
    }

    fn new_jump_if_false(&mut self) -> Jump {
        self.add_opcode(Opcode::JumpIfFalse(0));
        Jump(self.data.len() - 1)
    }

    fn new_call(&mut self) -> Jump {
        self.add_opcode(Opcode::Call(0));
        Jump(self.data.len() - 1)
    }

    fn new_spawn(&mut self, args: u8) -> Jump {
        self.add_opcode(Opcode::Spawn { args, target: 0 });
        Jump(self.data.len() - 1)
    }

    fn patch_jump(&mut self, jump: Jump, label: Label) {
        match &mut self.data[jump.0] {
            Opcode::Jump(target)
            | Opcode::JumpIfFalse(target)
            | Opcode::Call(target)
            | Opcode::Spawn { target, .. } => *target = label.0,
            opcode => panic!("Tried to patch {opcode:?} which isn't a jump"),
        }
    }

    pub fn compile(&self) -> Vec<u16> {
        let mut result = Vec::with_capacity(self.length);

        macro_rules! one_arg {
            ($kind:ident, $value:expr) => {
                result.push(((bytecode::Instruction::$kind as u16) << 8) | (($value as u8) as u16));
            };
        }

        for opcode in &self.data {
            match *opcode {
                Opcode::Push8(value) => {
                    one_arg!(Push8, value);
                }
                Opcode::Push32(value) => {
                    one_arg!(Push32, 0);
                    let bytes = value.to_le_bytes();
                    result.push(u16::from_le_bytes([bytes[0], bytes[1]]));
                    result.push(u16::from_le_bytes([bytes[2], bytes[3]]));
                }
                Opcode::Dup(amount) => {
                    one_arg!(Dup, amount);
                }
                Opcode::Drop(amount) => {
                    one_arg!(Drop, amount);
                }
                Opcode::GetProp(index) => {
                    one_arg!(GetProp, index);
                }
                Opcode::SetProp(index) => {
                    one_arg!(SetProp, index);
                }
                Opcode::Wait => {
                    one_arg!(Wait, 0);
                }
                Opcode::Move(amount) => {
                    one_arg!(Move, amount);
                }
                Opcode::MathsOp(op) => {
                    one_arg!(MathsOp, bytecode::MathsOp::from(op));
                }
                Opcode::JumpIfFalse(target) => {
                    one_arg!(JumpIfFalse, 0);
                    result.push(target);
                }
                Opcode::Jump(target) => {
                    one_arg!(Jump, 0);
                    result.push(target);
                }
                Opcode::Call(target) => {
                    one_arg!(Call, 0);
                    result.push(target);
                }
                Opcode::Spawn { args, target } => {
                    one_arg!(Spawn, args);
                    result.push(target);
                }
                Opcode::Return { args, rets, shift } => {
                    one_arg!(Return, args);
                    result.push(u16::from_be_bytes([rets, shift]));
                }
                Opcode::Trigger(index) => {
                    one_arg!(Trigger, index);
                }
            }
        }

        result
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Label(u16);
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Jump(usize);

#[cfg(test)]
mod test {
    use std::{fmt::Write, fs};

    use insta::{assert_snapshot, glob};

    use super::*;

    #[test]
    fn compiler_snapshot_tests() {
        glob!("snapshot_tests", "compiler/*.tapir", |path| {
            let input = fs::read_to_string(path).unwrap();

            let compiler_settings = CompileSettings {
                properties: vec![Property {
                    ty: Type::Int,
                    index: 0,
                    name: "int_prop".to_string(),
                }],
                enable_optimisations: false,
            };

            let bytecode = compile(path, &input, &compiler_settings).unwrap();
            let decompiled = print_opcodes(&bytecode.data);

            assert_snapshot!(decompiled);
        });
    }

    fn print_opcodes(opcodes: &[Opcode]) -> String {
        let mut result = String::new();

        let mut current_pos = 0;
        for opcode in opcodes {
            writeln!(&mut result, "{current_pos:08}: {opcode}").unwrap();
            current_pos += opcode.size();
        }

        result
    }
}
