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

type action2 =
  | ANoop
  | AStart(action2 => unit)
  | AStop
  | ASetDuration(string)
  | AGetCurrentTime
  | ASetTimeLeft
  | ASetTimer(option(Js.Global.intervalId), float)
  | ASetCurrentTime(float);


let startClock2 = (dispatch) => {
  Some(Js.Global.setInterval(() => dispatch(AGetCurrentTime), 30));
};


type effect =
  | IODoNothing
  | IOStartTimer(action2 => unit)
  | IOStopTimer(option(Js.Global.intervalId))
  | IOGetCurrentTime;

// TODO how can we extract/separate dispatch from state and business logic?
type businessLogic = (state, action2) => (state, effect);
let businessLogic = (state, action) => {
  switch (action) {
    | AStart(dispatch) => (state, IOStartTimer(dispatch));
    | AStop => ({ ...state, durationInput: -1.0}, IOStopTimer(state.intervalId));
    | ASetDuration(v) => ({ ...state, durationInput: float_of_string(v) }, IODoNothing);
    | ASetTimer(intervalId, t) => ({ ...state, intervalId: intervalId, timerStartTime: t }, IODoNothing)
    | AGetCurrentTime => (state, IOGetCurrentTime);
    | ASetCurrentTime(t) => (setTimeLeft({ ...state, currentTime: Some(t)}), IODoNothing);
    | ANoop => (state, IODoNothing);
  };
}

type runEffect = (effect) => action2;
let runEffect = (effect) => {
  switch (effect) {
    | IODoNothing => ANoop;
    | IOStartTimer(dispatch) => ASetTimer(startClock2(dispatch), Js.Date.now());
    | IOStopTimer(intervalId) => ASetTimer(stopClock(intervalId), -1.0);
    | IOGetCurrentTime => ASetCurrentTime(Js.Date.now());
  }
};

let rec wrapBusinessLogicWithEffects = (f, state, action2) => {
  let (newState, effect) = f(state, action2);
  if (effect == IODoNothing) {
    (newState, ANoop);
  } else {
    let nextAction = runEffect(effect);
    wrapBusinessLogicWithEffects(f, newState, nextAction);
  };
};
