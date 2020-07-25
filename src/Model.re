type state = {
  durationInput: string,
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
  | IOGetCurrentTime
  | IOCheckIfTimerFinished(option(float));

let initState = {
  durationInput: "",
  timerStartTime: None,
  currentTime: None,
  timeLeft: None,
  intervalId: None
};

let setDuration = (state, input) => {
  ({ ...state, durationInput: input}, IODoNothing);
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
  let duration = Belt.Float.fromString(durationInput);
  let start = Belt.Option.(
    map(timerStartTime, getWithDefault(currentTime))
    );
  let end_ = Option.lift2(calcEndTime, timerStartTime, duration); //Belt.Option.map(timerStartTime, (x) => calcEndTime(x, durationInput));
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

let setTimeLeft = (state, t) => {
  let newState = setTimeLeft({ ...state, currentTime: Some(t)});
  let { timeLeft } = newState;
  // TODO this is arguably not a side-effect in itself, it is an action but a
  // limitation of current abstraction is that every action has an effect and
  // the action-effect pairing is linear with no branching
  (newState, IOCheckIfTimerFinished(timeLeft));
}

// TODO how can we extract/separate dispatch from state and business logic?
// TODO look at reductive
type businessLogic = (state, action) => (state, effect);
let businessLogic = (state, action) => {
  switch (action) {
    | Start(dispatch) => (state, IOStartTimer(dispatch));
    | Stop => ({ ...state, durationInput: ""}, IOStopTimer(state.intervalId));
    | SetDuration(v) => setDuration(state, v);
    | SetTimer(intervalId, t) => ({ ...state, intervalId: intervalId, timerStartTime: t }, IODoNothing)
    | GetCurrentTime => (state, IOGetCurrentTime);
    | SetCurrentTime(t) => setTimeLeft(state, t);
    | Noop => (state, IODoNothing);
  };
}

let startTimer = (dispatch) => {
  Js.log("starting timer...");
  SetTimer(startClock(dispatch), Some(Js.Date.now()));
}

let isFinished = (timeLeft) => {
  switch (timeLeft) {
    | Some(t) => if (t <= 0.0) {
      Stop;
    } else {
      Noop;
    };
    | None => Noop; // strictly an exception
  }
}

type runEffect = (effect) => action;
let runEffect = (effect) => {
  switch (effect) {
    | IODoNothing => Noop;
    | IOStartTimer(dispatch) => startTimer(dispatch);
    | IOStopTimer(intervalId) => SetTimer(stopClock(intervalId), None);
    | IOGetCurrentTime => SetCurrentTime(Js.Date.now())
    | IOCheckIfTimerFinished(timeLeft) => isFinished(timeLeft);
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
