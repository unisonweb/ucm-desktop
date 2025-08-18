function listenToSystemChange() {
  window
    .matchMedia("(prefers-color-scheme: dark)")
    .addEventListener("change", ({ matches }) => {
      if (matches) {
        mount("unison-dark");
      } else {
        mount("unison-light");
      }
    });
}

function systemToActual() {
  if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
    return Promise.resolve("unison-dark");
  } else if (window.matchMedia("(prefers-color-scheme: light)").matches) {
    return Promise.resolve("unison-light");
  } else {
    return Promise.resolve("unison-dark");
  }
}

function equals(a, b) {
  return a === b;
}

function prettyName(theme) {
  if (theme === "unison-light") {
    return "Unison Light";
  } else if (theme === "unison-dark") {
    return "Unison Dark";
  } else {
    return "System";
  }
}

async function mount(theme) {
  let theme_ = theme;

  console.log("Setting theme:", theme);

  if (theme === "system") {
    theme_ = await systemToActual();
  }

  const $body = document.querySelector("body");

  if ($body) {
    $body.classList.remove("unison-light");
    $body.classList.remove("unison-dark");
    $body.classList.add(theme_);
  }
}

export { systemToActual, prettyName, equals, mount, listenToSystemChange };
