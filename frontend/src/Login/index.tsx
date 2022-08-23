import React, { useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { TOKEN_KEY } from "../constants";
import { useAuth } from "../context/authContext";

const Login = () => {
  const auth = useAuth();
  const navigate = useNavigate();

  const handleLogin = async (user: string | null) => {
    await auth?.login(user);
    navigate("/", { replace: true });
  };

  useEffect(() => {
    // check for local storage for api id or code
    const params = new URLSearchParams(window.location.search);
    if (localStorage.getItem(TOKEN_KEY) || params.get("code")) {
      const code: string | null = params.get("code");
      handleLogin(code);
    }
    //user does not grant permission, redirect to home
    else if (params.get("error")) window.location.replace("/");
    // window.location.href will cause infinite redirect loop
    else
      window.location.replace(
        `${process.env.REACT_APP_API_HOST_42}/oauth/authorize?client_id=${process.env.REACT_APP_API_UID_42}&redirect_uri=${encodeURIComponent(process.env.REACT_APP_API_REDIR_URI_42 as string)}&response_type=code`
      );
  }, []);

  return <div>Redirecting.. please wait</div>;
};

export default Login;
