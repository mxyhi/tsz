use crate::TszError;
use cranelift_module::{DataDescription, DataId, Linkage, Module as _};
use cranelift_object::ObjectModule;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub(super) struct StringPool {
    next_id: usize,
    ids: HashMap<String, DataId>,
}

impl StringPool {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn get_or_define_data(&mut self, object_module: &mut ObjectModule, s: &str) -> Result<DataId, TszError> {
        if let Some(id) = self.ids.get(s).copied() {
            return Ok(id);
        }

        let name = format!("__tsz_str_{}", self.next_id);
        self.next_id += 1;
        let data_id = object_module
            .declare_data(&name, Linkage::Local, false, false)
            .map_err(|e| TszError::Codegen {
                message: format!("declare data failed: {name}: {e}"),
            })?;

        let mut data = DataDescription::new();
        data.define(s.as_bytes().to_vec().into_boxed_slice());
        object_module.define_data(data_id, &data).map_err(|e| TszError::Codegen {
            message: format!("define data failed: {name}: {e}"),
        })?;

        self.ids.insert(s.to_string(), data_id);
        Ok(data_id)
    }
}

