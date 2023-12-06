use std::collections::HashMap;

use crate::*;

pub type Scope = Vec<HashMap<String, VariableValue>>;

pub fn enter_scope(scope: &mut Scope) {
    scope.push(HashMap::new())
}

pub fn exit_scope(scope: &mut Scope) {
    scope.pop();
}

pub fn get_var_from_scope<'a>(
    scope: &'a mut Scope,
    var_name: &str,
) -> Result<&'a mut VariableValue, Command> {
    scope
        .iter_mut()
        .rev()
        .find_map(|map| map.get_mut(var_name))
        .ok_or(Command::Error("variable is not defined".into()))
}

pub fn get_var_from_scope_cloned(scope: &Scope, var_name: &str) -> Result<VariableValue, Command> {
    scope
        .iter()
        .rev()
        .find_map(|map| map.get(var_name).cloned())
        .ok_or(Command::Error("variable is not defined".into()))
}

pub fn define_var_in_scope(
    scope: &mut Scope,
    var_name: &str,
    val: VariableValue,
) -> Result<(), Command> {
    if let Some(_) = scope
        .last_mut()
        .expect("there should always be a layer")
        .insert(var_name.to_string(), val)
    {
        Err(Command::Error("variable was already defined".into()))
    } else {
        Ok(())
    }
}

pub fn assign_var_in_scope(
    scope: &mut Scope,
    var_name: &str,
    val: VariableValue,
) -> Result<(), Command> {
    scope
        .iter_mut()
        .rev()
        .find_map(|map| map.get_mut(var_name))
        .map(|v| *v = val)
        .ok_or(Command::Error("".into()))
}
