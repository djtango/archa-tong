open ReactNative;

/* type state = Model.state; */
[@react.component]
let v = () => {
  let (state, dispatch) = React.useReducer(Model.reducer, {durationInput: -1.0, timerStartTime: -1.0});
  <View>
    <Text>
      (string_of_float(state.durationInput))->React.string
    </Text>
    <TextInput placeholder={"time in ms"}
               onChange={(changeEvent => dispatch( Model.SetDuration(changeEvent##nativeEvent##text) ))}
               >
    </TextInput>
    <TouchableOpacity onPress={_ => dispatch( Model.SetDuration(string_of_float(state.durationInput -. 1.0)) )}>
      <Text>
        "Start"->React.string
      </Text>
    </TouchableOpacity>
 </View>
};
