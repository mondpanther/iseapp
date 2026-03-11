$(document).on("shiny:connected", () => {
  console.log("Custom JS Script loaded...(Shiny Connected)");
});

$(document).on("shiny:sessioninitialized", () => {
  console.log("Custom JS Script loaded...(Session Initialized)");
});

