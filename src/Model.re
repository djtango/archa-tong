type state = {
  durationInput: float,
  timerStartTime: float,
  currentTime: option(float),
  timeLeft: option(float),
  intervalId: option(Js.Global.intervalId)
};

type action =
  | Noop
  | Start(action => unit)
  | Stop
  | SetDuration(string)
  | GetCurrentTime
  | SetTimeLeft
  | SetTimer(option(Js.Global.intervalId), float)
  | SetCurrentTime(float);

type effect =
  | IODoNothing
  | IOStartTimer(action => unit)
  | IOStopTimer(option(Js.Global.intervalId))
  | IOGetCurrentTime;

let initState = {
  durationInput: -1.0,
  timerStartTime: -1.0,
  currentTime: None,
  timeLeft: None,
  intervalId: None
};

let timeLeft = (currentTime, endTime) => {
  endTime -. currentTime;
};

let calcEndTime = (startTime, duration) => {
  startTime +. duration;
};

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

// TODO how can we extract/separate dispatch from state and business logic?
type businessLogic = (state, action) => (state, effect);
let businessLogic = (state, action) => {
  switch (action) {
    | Start(dispatch) => (state, IOStartTimer(dispatch));
    | Stop => ({ ...state, durationInput: -1.0}, IOStopTimer(state.intervalId));
    | SetDuration(v) => ({ ...state, durationInput: float_of_string(v) }, IODoNothing);
    | SetTimer(intervalId, t) => ({ ...state, intervalId: intervalId, timerStartTime: t }, IODoNothing)
    | GetCurrentTime => (state, IOGetCurrentTime);
    | SetCurrentTime(t) => (setTimeLeft({ ...state, currentTime: Some(t)}), IODoNothing);
    | Noop => (state, IODoNothing);
  };
}

type runEffect = (effect) => action;
let runEffect = (effect) => {
  switch (effect) {
    | IODoNothing => Noop;
    | IOStartTimer(dispatch) => SetTimer(startClock(dispatch), Js.Date.now());
    | IOStopTimer(intervalId) => SetTimer(stopClock(intervalId), -1.0);
    | IOGetCurrentTime => SetCurrentTime(Js.Date.now());
  }
};

let rec wrapBusinessLogicWithEffects = (f, state, action) => {
  let (newState, effect) = f(state, action);
  if (effect == IODoNothing) {
    (newState, Noop);
  } else {
    let nextAction = runEffect(effect);
    wrapBusinessLogicWithEffects(f, newState, nextAction);
  };
};
