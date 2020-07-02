let timeLeft = (currentTime, endTime) => {
  endTime -. currentTime;
};

let calcEndTime = (startTime, duration) => {
  startTime +. duration;
};

type state = {
  durationInput: float,
  timerStartTime: float,
  currentTime: option(float),
  timeLeft: option(float),
  intervalId: option(Js.Global.intervalId)
};

let initState = {
  durationInput: -1.0,
  timerStartTime: -1.0,
  currentTime: None,
  timeLeft: None,
  intervalId: None
};
/* type dispatch('a) = ('a) => unit; */

type action =
  | Noop
  | Start(action => unit)
  | Stop
  | SetDuration(string)
  | GetCurrentTime
  | SetTimeLeft;



/* type action = { */
/*   name: actions, */
/*   value: float */
/* } */

let setTimeLeft = (state) => {
  let start = Belt.Option.getWithDefault(state.currentTime, state.timerStartTime);
  let end_ = calcEndTime(state.timerStartTime, state.durationInput);
  {
    ...state,
    timeLeft: Some(timeLeft(start, end_))
  }
}

let startClock = (dispatch) => {
  Some(Js.Global.setInterval(() => dispatch(GetCurrentTime), 30));
};

let stopClock = (intervalId) => {
  ignore(Belt.Option.map(intervalId, Js.Global.clearInterval));
  None;
}

type reducer = (state, action) => (state, action);
let reducer = (state, action) => {
  Js.log("reducer");
  Js.log(state);
  Js.log(action);
  let newState = switch (action) {
    | Start(dispatch) => { ...state, intervalId: startClock(dispatch), timerStartTime: Js.Date.now()};
    | Stop => { ...state, durationInput: -1.0, intervalId: stopClock(state.intervalId) };
    | SetDuration(v) => { ...state, durationInput: float_of_string(v) };
    | GetCurrentTime => { ...state, currentTime: Some(Js.Date.now()) };
    | SetTimeLeft => setTimeLeft(state);
    | Noop => state;
  };
  let newAction = switch (action) {
    | Start(_) => GetCurrentTime;
    | Stop => Noop;
    | SetDuration(_) => Noop;
    | GetCurrentTime => SetTimeLeft;
    | SetTimeLeft => Noop;
    | Noop => Noop;
  };
  (newState, newAction);
};

/* type withCoeffect = (reducer, state, action) => state; */
let rec withCoeffect = (f, state, action) => {
  let (newState, nextAction) = f(state, action);
  if (nextAction == Noop) {
    (newState, Noop);
  } else {
    withCoeffect(f, newState, nextAction);
  };
}

/* Js.Date.( now() |> fromFloat |> Js.log ); */

let greetMore = (name) => {
  let part1 = "Hello";
  part1 ++ " " ++ name;
};
