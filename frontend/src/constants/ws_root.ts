//assuming that api is same source as 42 auth redireted url
const redirText: string =
  process.env.REACT_APP_API_REDIR_URI_42 || "http://localhost:3000/login";

export const WS_ROOT = `http://${redirText.substring(
  redirText.indexOf("http://") + 7,
  redirText.indexOf(":", 7) > 0
    ? redirText.indexOf(":", 7)
    : redirText.indexOf("/login", 7)
)}:3001/api/v1`;
