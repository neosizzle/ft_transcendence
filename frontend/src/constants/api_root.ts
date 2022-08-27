//assuming that api is same source as 42 auth redireted url
const redirText : string = process.env.REACT_APP_API_REDIR_URL_42 || "http://localhost:3000/login"

export const API_ROOT = `http://${redirText.substring(redirText.indexOf("http://") + 7, redirText.indexOf(":", 7))}:3001/api/v1`
