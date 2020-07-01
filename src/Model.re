let timeLeft = (currentTime, endTime) => {
  endTime - currentTime;
};

let calcEndTime = (startTime, duration) => {
  startTime + duration;
};

type state = {
  durationInput: float,
  timerStartTime: float
};

type action =
  | Start
  | Stop
  | SetDuration(string);

/* type action = { */
/*   name: actions, */
/*   value: float */
/* } */

type reducer = (state, action) => state;
let reducer = (state, action) => {
  switch (action) {
    | Start => { ...state, timerStartTime: Js.Date.now() }
    | Stop => { ...state, durationInput: -1.0 }
    | SetDuration(v) => { ...state, durationInput: float_of_string(v) }
  }
};
/*  */
/* Js.Date.( now() |> fromFloat |> Js.log ); */

let greetMore = (name) => {
  let part1 = "Hello";
  part1 ++ " " ++ name;
};
