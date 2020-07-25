open ReactNative;

let presentDuration = (d) => {
  Js.Float.toString(d);
};

module Duration = {
  [@react.component]
  let v = (~state: Model.state) =>{
    let t = Belt.Option.getWithDefault(state.timeLeft, Js.Float.fromString(state.durationInput));
    <Text>
      (presentDuration(t))->React.string
    </Text>
  };
}

/* type state = Model.state; */
[@react.component]
let v = () => {
  let r = (a, b) => {
    let (state, _) = Model.wrapBusinessLogicWithEffects(Model.businessLogic, a, b);
    state;
  };
  let (state, dispatch) = React.useReducer(r, Model.initState);
  let f = (dispatch) => (a) => {
    ignore(dispatch(a));
    ();
  };
  <View>
    <Duration.v state={state}/>
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
