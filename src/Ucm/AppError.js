function init(ex) {
  const $errWrapper = document.createElement("div");

  const $err = document.createElement("div");
  $err.className = "app-error";
  const $errHeader = document.createElement("h2");
  $errHeader.innerHTML = "Could not start application";
  $err.appendChild($errHeader);
  const $errMessage = document.createElement("p");

  if (typeof ex === "string") {
    $errMessage.innerHTML = ex;
  }
  else if (ex instanceof Error) {
    $errMessage.innerHTML = ex.message
  }
  else {
    $errMessage.innerHTML = `An unknown error occured: ${ex}`;
  }

  $err.appendChild($errMessage);
  $errWrapper.appendChild($err);

  return $errWrapper;
}

function mount($err) {
  const $body = document.querySelector("body");
  if ($body) {
    $body.appendChild($err);
  }
}

export { init, mount }
