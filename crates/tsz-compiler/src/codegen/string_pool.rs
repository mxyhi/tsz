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

        let mut blob = Vec::with_capacity(8 + s.as_bytes().len());
        let len = i64::try_from(s.as_bytes().len()).map_err(|_| TszError::Codegen {
            message: "String literal too long".to_string(),
        })?;
        // String layout: [i64 len][u8 bytes...]. A string value points to the first byte (i.e. base+8).
        blob.extend_from_slice(&len.to_ne_bytes());
        blob.extend_from_slice(s.as_bytes());

        let mut data = DataDescription::new();
        data.define(blob.into_boxed_slice());
        data.set_align(8);
        object_module.define_data(data_id, &data).map_err(|e| TszError::Codegen {
            message: format!("define data failed: {name}: {e}"),
        })?;

        self.ids.insert(s.to_string(), data_id);
        Ok(data_id)
    }
}
