use std::fmt::{self, Write};

use bytecode::{Opcode, Type1, Type3};

fn disassemble_non_constant(instr: u32, target: &mut dyn Write) -> fmt::Result {
    macro_rules! t1 {
        ($text:expr, 0) => {
            writeln!(target, "{}", $text)
        };
        ($text:expr, 1) => {{
            let t1 = Type1::decode(instr);
            writeln!(target, "{} {}", $text, t1.target)
        }};
        ($text:expr, 2) => {{
            let t1 = Type1::decode(instr);
            writeln!(target, "{} {}, {}", $text, t1.target, t1.a)
        }};
        ($text:expr, 3) => {{
            let t1 = Type1::decode(instr);
            writeln!(target, "{} {}, {}, {}", $text, t1.target, t1.a, t1.b)
        }};
    }

    match bytecode::opcode(instr)
        .unwrap_or_else(|| panic!("Invalid opcode for instruction {instr}"))
    {
        bytecode::Opcode::Mov => t1!("mov", 2),
        bytecode::Opcode::Add => t1!("add", 3),
        bytecode::Opcode::Sub => t1!("sub", 3),
        bytecode::Opcode::Mul => t1!("mul", 3),
        bytecode::Opcode::RealMod => t1!("realmod", 3),
        bytecode::Opcode::RealDiv => t1!("realdiv", 3),
        bytecode::Opcode::EqEq => t1!("eq", 3),
        bytecode::Opcode::NeEq => t1!("neq", 3),
        bytecode::Opcode::Gt => t1!("gt", 3),
        bytecode::Opcode::GtEq => t1!("gte", 3),
        bytecode::Opcode::Lt => t1!("lt", 3),
        bytecode::Opcode::LtEq => t1!("lte", 3),
        bytecode::Opcode::FixMul => t1!("fixmul", 3),
        bytecode::Opcode::FixDiv => t1!("fixdiv", 3),
        bytecode::Opcode::GetProp => t1!("getprop", 2),
        bytecode::Opcode::SetProp => t1!("setprop", 2),
        bytecode::Opcode::Call => t1!("call", 1),
        bytecode::Opcode::Spawn => t1!("spawn", 2),
        bytecode::Opcode::Trigger => t1!("trigger", 2),
        bytecode::Opcode::JumpIf => t1!("jumpif", 1),
        bytecode::Opcode::Ret => t1!("ret", 0),
        bytecode::Opcode::Wait => t1!("wait", 0),
        bytecode::Opcode::LoadConstant => unreachable!("Load constant should not be hit here"),

        bytecode::Opcode::Jump => {
            let t3 = Type3::decode(instr);
            writeln!(target, "jmp {}", t3.value)
        }
    }
}

pub fn disassemble(instrs: &[u32], target: &mut dyn Write) -> fmt::Result {
    let mut is_constant = false;

    for (i, instr) in instrs.iter().enumerate() {
        if is_constant {
            writeln!(target, "{instr}")?;
            is_constant = false;
            continue;
        }

        write!(target, "{i:08}: ")?;
        if bytecode::opcode(*instr) == Some(Opcode::LoadConstant) {
            let t1 = Type1::decode(*instr);
            write!(target, "loadk {}, ", t1.target)?;
            is_constant = true;
        } else {
            disassemble_non_constant(*instr, target)?;
        }
    }

    Ok(())
}
