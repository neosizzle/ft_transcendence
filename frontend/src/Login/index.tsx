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
        "https://api.intra.42.fr/oauth/authorize?client_id=9bb9d1702c9ddda04e2b99da9b32d7ecfd7be565846ed56b2055f25d51221261&redirect_uri=http%3A%2F%2Flocalhost%3A3000%2Flogin&response_type=code"
      );
  }, []);

  return <div>Redirecting.. please wait</div>;
};

export default Login;
