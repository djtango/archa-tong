open ReactNative;

let presentDuration = (d: float) => {
  let minutes = Js.Math.floor(d /. 60000.0);
  let seconds = Js.Math.floor(d /. 1000.0) mod 60;
  let millis = Js.Math.floor(d) mod 1000;
  let s = Js.Int.toString;
  s(minutes) ++ ":" ++ s(seconds) ++ "." ++ s(millis);
};

let durationInputToTime = Js.Float.fromString;

module TimeLeft = {
  [@react.component]
  let v = (~state: Model.state) =>{
    let t = Belt.Option.getWithDefault(state.timeLeft, durationInputToTime(state.durationInput));
    <Text>
      (presentDuration(t))->React.string
    </Text>
  };
}

/* type state = Model.state; */
[@react.component]
let v = () => {
  let (state, dispatch) = React.useReducer(Model.reducer, Model.initState);
  let f = (dispatch) => (a) => {
    ignore(dispatch(a));
    ();
  };
  <View>
    <TimeLeft.v state={state}/>
    <TextInput placeholder={"time in ms"} // TODO use a real time picker
               onChange={(changeEvent => dispatch( Model.SetDuration(changeEvent##nativeEvent##text) ))}>
    </TextInput>
    <TouchableOpacity onPress={_ => dispatch(Model.Start(f(dispatch)))}>
      <Text>
        "Start"->React.string
      </Text>
    </TouchableOpacity>
    <TouchableOpacity onPress={_ => dispatch(Model.Stop)}>
      <Text>
        "Stop"->React.string
      </Text>
    </TouchableOpacity>
 </View>
};
