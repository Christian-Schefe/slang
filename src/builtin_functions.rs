use crate::variables::VariableValue;

pub fn try_get_builtin_function(
    target: VariableValue,
    function_name: String,
) -> Option<VariableValue> {
    match target {
        VariableValue::String(ref _s) => match function_name.as_str() {
            "split" => Some(VariableValue::BuiltinFunction(
                Box::new(target),
                function_name,
            )),
            _ => None,
        },
        _ => None,
    }
}
