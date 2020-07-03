open ReactNative;

let presentDuration = (d) => {
  /* Js.Float.toString(d /. 1000.); */
  Js.Float.toString(d);
};

module Duration = {
  [@react.component]
  let v = (~state: Model.state) =>{
    let t = Belt.Option.getWithDefault(state.timeLeft, state.durationInput);
    <Text>
      (presentDuration(t))->React.string
    </Text>
  };
}

/* type state = Model.state; */
[@react.component]
let v = () => {
  let r = (a, b) => {
    /* let (state, _) = Model.withCoeffect(Model.reducer, a, b); */
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
    <TextInput placeholder={"time in ms"}
               onChange={(changeEvent => dispatch( Model.ASetDuration(changeEvent##nativeEvent##text) ))}>
    </TextInput>
    <TouchableOpacity onPress={_ => dispatch(Model.AStart(f(dispatch)))}>
      <Text>
        "Start"->React.string
      </Text>
    </TouchableOpacity>
    <TouchableOpacity onPress={_ => dispatch(Model.AStop)}>
      <Text>
        "Stop"->React.string
      </Text>
    </TouchableOpacity>
 </View>
};
