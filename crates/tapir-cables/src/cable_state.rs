use std::{
    collections::HashMap,
    hash::Hash,
    sync::{Arc, Mutex},
};

#[derive(Clone, Debug)]
pub struct CableState<BlockId: crate::BlockId> {
    inner: Arc<Mutex<CableStateInner<BlockId>>>,
}

impl<BlockId: crate::BlockId> Default for CableState<BlockId> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<BlockId: crate::BlockId + 'static> CableState<BlockId> {
    pub fn from_ctx<F, T>(ctx: &egui::Context, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        ctx.data_mut(|data| f(data.get_temp_mut_or_default::<Self>(egui::Id::null())))
    }

    pub fn set_port_position(&mut self, port_id: &PortId<BlockId>, position: egui::Pos2) {
        self.inner
            .lock()
            .unwrap()
            .port_positions
            .insert(port_id.clone(), position);
    }

    pub fn clear_port_positions(&mut self) {
        self.inner.lock().unwrap().port_positions.clear();
    }

    pub fn get_port_position(&self, port_id: &PortId<BlockId>) -> Option<egui::Pos2> {
        self.inner
            .lock()
            .unwrap()
            .port_positions
            .get(port_id)
            .copied()
    }

    pub fn set_in_progress_cable(&mut self, port_id: &PortId<BlockId>) {
        self.inner
            .lock()
            .unwrap()
            .in_progress_cable
            .get_or_insert(port_id.clone());
    }

    pub fn clear_in_progress_cable(&mut self) {
        self.inner.lock().unwrap().in_progress_cable = None;
    }

    pub fn in_progress_cable(&mut self) -> Option<(egui::Pos2, PortId<BlockId>)> {
        let mut inner = self.inner.lock().unwrap();
        let in_progress_cable = inner.in_progress_cable.as_ref()?;

        let Some(pos) = inner.port_positions.get(in_progress_cable).cloned() else {
            inner.in_progress_cable = None;
            return None;
        };

        Some((pos, in_progress_cable.clone()))
    }

    pub fn closest_port_at_pos(&self, pos: egui::Pos2) -> Option<(PortId<BlockId>, egui::Pos2)> {
        let inner = self.inner.lock().unwrap();

        let (port, pos) = inner.port_positions.iter().min_by(|a, b| {
            a.1.distance_sq(pos)
                .partial_cmp(&b.1.distance_sq(pos))
                .expect("Distance is NaN")
        })?;

        Some((port.clone(), *pos))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub struct PortId<BlockId: Hash> {
    pub block_id: BlockId,
    pub index: usize,
    pub direction: super::PortDirection,
}

impl<BlockId: Hash> PortId<BlockId> {
    pub fn new(block_id: BlockId, index: usize, direction: super::PortDirection) -> Self {
        Self {
            block_id,
            index,
            direction,
        }
    }
}

#[derive(Debug)]
struct CableStateInner<BlockId: crate::BlockId> {
    port_positions: HashMap<PortId<BlockId>, egui::Pos2>,
    in_progress_cable: Option<PortId<BlockId>>,
}

impl<BlockId: crate::BlockId> Default for CableStateInner<BlockId> {
    fn default() -> Self {
        Self {
            port_positions: HashMap::new(),
            in_progress_cable: None,
        }
    }
}
