let input = Node.Fs.readFileAsUtf8Sync("./src/day2-input.txt");

let testApp = Js.String.split(",", input) |> Array.map(int_of_string);

let operation = (f, fst, snd, register, program) => {
  let result = f(program[fst], program[snd]);
  let copy = Array.copy(program);
  copy[register] = result;
  copy;
};

let add = operation((+));
let multiply = operation(( * ));

let runProgram = program => {
  let rec loop = (pointer, program) => {
    let opCode = program[pointer];
    let runOperation = f => {
      loop(
        pointer + 4,
        f(
          program[pointer + 1],
          program[pointer + 2],
          program[pointer + 3],
          program,
        ),
      );
    };
    switch (opCode) {
    | 99 => program
    | 1 => runOperation(add)
    | 2 => runOperation(multiply)
    | _ => program
    };
  };
  loop(0, program);
};

let findNounAndVerb = (program, output) => {
  let rec loop = (noun, verb) => {
    let attempt = Array.copy(program);
    attempt[1] = noun;
    attempt[2] = verb;
    if (runProgram(attempt)[0] == output) {
      (noun, verb);
    } else if (noun < 99) {
      loop(noun + 1, verb);
    } else if (verb < 99) {
      loop(0, verb + 1);
    } else {
      ((-1), (-1));
    };
  };
  loop(0, 0);
};

Js.log(runProgram(testApp)[0]);
Js.log(findNounAndVerb(testApp, 19690720));