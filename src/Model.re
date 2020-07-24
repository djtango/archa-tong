type state = {
  durationInput: float,
  timerStartTime: option(float),
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
  | SetTimer(option(Js.Global.intervalId), option(float))
  | SetCurrentTime(float);

type effect =
  | IODoNothing
  | IOStartTimer(action => unit)
  | IOStopTimer(option(Js.Global.intervalId))
  | IOGetCurrentTime;

let initState = {
  durationInput: -1.0,
  timerStartTime: None,
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

module Option = {
  // TODO look at Rationale
  let pure_ = (x) => Some(x);
  let apply = (optionF, optionO) => {
    switch ((optionF, optionO)) {
      | (Some(f), Some(o)) => Some(f(o));
      | (Some(_), None) => None;
      | (None, Some(_)) => None;
      | (None, None) => None;
    };
  };
  let lift2 = (f, oA, oB) => {
    apply(apply((pure_(f)), oA), oB);
  }
};

let setTimeLeft = (state) => {
  let { timerStartTime, currentTime, durationInput } = state;
  let start = Belt.Option.(
    map(timerStartTime, getWithDefault(currentTime))
    );
  let end_ = Belt.Option.map(timerStartTime, (x) => calcEndTime(x, durationInput));
  {
    ...state,
    timeLeft: Option.lift2(timeLeft, start, end_)
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
// TODO look at reductive
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

let startTimer = (dispatch) => {
  Js.log("starting timer...");
  SetTimer(startClock(dispatch), Some(Js.Date.now()));
}

type runEffect = (effect) => action;
let runEffect = (effect) => {
  switch (effect) {
    | IODoNothing => Noop;
    | IOStartTimer(dispatch) => startTimer(dispatch);
    | IOStopTimer(intervalId) => SetTimer(stopClock(intervalId), None);
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
