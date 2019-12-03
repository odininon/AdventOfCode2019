let weights =
  Node.Fs.readFileAsUtf8Sync("./src/day1-input.txt")
  |> Js.String.split("\n")
  |> Array.map(int_of_string);

let fuelNeeded = weight => {
  weight / 3 - 2;
};

let totalFuelNeeded = weight => {
  let rec _do = (weight, component) => {
    let fuelWeight = fuelNeeded(component);
    if (fuelWeight > 0) {
      _do(fuelWeight + weight, fuelWeight);
    } else {
      weight;
    };
  };
  _do(0, weight);
}

let totalWeight = weights => Belt.Array.reduce(weights, 0, (+));

weights |> Array.map(totalFuelNeeded) |> totalWeight |> Js.log;