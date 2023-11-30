use std::{collections::HashMap, fmt::Debug};

use crate::variables::*;

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: HashMap<String, VariableValue>,
}

#[derive(Debug)]
pub struct Context {
    pub layers: Vec<Scope>,
    pub cur_layer: usize,
}

impl Context {
    pub fn current_scope(&mut self) -> &mut Scope {
        &mut self.layers[self.cur_layer]
    }
    pub fn define_var(&mut self, var: &String, val: VariableValue) -> Result<(), RuntimeError> {
        self.current_scope().define_var(var, val)
    }

    pub fn try_get_var(&mut self, var: &String) -> Option<(usize, &mut VariableValue)> {
        self.layers
            .iter_mut()
            .enumerate()
            .rev()
            .find_map(|(i, layer)| layer.try_get_var(var).map(|v| (i, v)))
    }

    pub fn try_set_var(&mut self, var: &String, val: VariableValue) -> Option<()> {
        self.layers
            .iter_mut()
            .rev()
            .find_map(|layer| layer.try_set_var(var, val.clone()))
    }

    pub fn get_var(&mut self, var: &String) -> Result<(usize, &mut VariableValue), RuntimeError> {
        self.try_get_var(var)
            .ok_or(RuntimeError(format!("Variable '{:?}' does not exist", var)))
    }

    pub fn set_var(&mut self, var: &String, val: VariableValue) -> Result<(), RuntimeError> {
        self.try_set_var(var, val)
            .ok_or(RuntimeError(format!("Variable '{:?}' does not exist", var)))
    }

    pub fn create_subcontext(&self, layer: usize) -> Result<Context, RuntimeError> {
        let mut new_layers = self.layers[0..layer + 1].to_vec();
        new_layers.push(Scope::new());
        Ok(Context {
            layers: new_layers,
            cur_layer: layer + 1,
        })
    }

    pub fn apply_subcontext(&mut self, layer: usize, context: Context) -> Result<(), RuntimeError> {
        for scope_i in 0..layer + 1 {
            let scope_to_update = &mut self.layers[scope_i];
            let updated_scope = &context.layers[scope_i];
            for (key, val) in &updated_scope.variables {
                scope_to_update
                    .try_set_var(key, val.clone())
                    .ok_or(RuntimeError(format!("Error applying context")))?;
            }
        }
        Ok(())
    }

    pub fn create_block_context(&self) -> Result<Context, RuntimeError> {
        self.create_subcontext(self.cur_layer)
    }

    pub fn apply_block_context(&mut self, context: Context) -> Result<(), RuntimeError> {
        self.apply_subcontext(self.cur_layer, context)
    }
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
        }
    }
    pub fn define_var(&mut self, var: &String, val: VariableValue) -> Result<(), RuntimeError> {
        if self.variables.contains_key(var) {
            return Err(RuntimeError(format!("Variable '{}' already exists", var)));
        }
        self.variables.insert(var.to_string(), val);
        Ok(())
    }

    pub fn try_get_var(&mut self, var: &String) -> Option<&mut VariableValue> {
        self.variables.get_mut(var)
    }

    pub fn try_set_var(&mut self, var: &String, val: VariableValue) -> Option<()> {
        if let Some(v) = self.variables.get_mut(var) {
            *v = val;
            Some(())
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError(pub String);

#[derive(Debug)]
pub struct SyntaxError(pub String);

#[derive(Debug)]
pub struct ClientError(pub String);
