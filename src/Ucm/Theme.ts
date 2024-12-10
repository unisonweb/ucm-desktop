export type Theme = "unison-light" | "unison-dark" | "system";

function listenToSystemChange() {
  window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", ({ matches }) => {
    if (matches) {
      mount("unison-dark");
    }
    else {
      mount("unison-light");
    }
  })
}

function systemToActual(): Promise<Theme> {
  if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
    return Promise.resolve("unison-dark");
  }
  else if (window.matchMedia("(prefers-color-scheme: light)").matches) {
    return Promise.resolve("unison-light");
  }
  else {
    return Promise.resolve("unison-dark");
  }
}

function equals(a: Theme, b: Theme): boolean {
  return a === b;
}

function prettyName(theme: Theme): string {
  if (theme === "unison-light") {
    return "Unison Light";
  }
  else if (theme === "unison-dark") {
    return "Unison Dark";
  }
  else {
    return "System";
  }
}

async function mount(theme: Theme) {
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

export { systemToActual, prettyName, equals, mount, listenToSystemChange }
