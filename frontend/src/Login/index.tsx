import React, { useEffect, useState } from 'react'
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../context/authContext';

const Login = (props: any) => {
	const auth = useAuth();
	const navigate = useNavigate();

	const handleLogin = (user : string) =>
	{
		auth?.login(user)
		navigate("/", {replace : true})
	}

	useEffect(() => {
		// check for local storage for api id or code
		const params = new URLSearchParams(window.location.search)
		if (localStorage.getItem("trans-token") || params.get('code'))
		{
			const code : string  | null = params.get('code');
			if (code) handleLogin(code);
		}
		//user does not grant permission, redirect to home
		else if (params.get('error'))
			window.location.replace("/")
		else
			// window.location.href will cause infinite redirect loop
			window.location.replace("https://api.intra.42.fr/oauth/authorize?client_id=5d614e506c4c77f5174eb5107dc4ccc21c425fa78cdb4e8b00fe69e2dc95b128&redirect_uri=http%3A%2F%2Flocalhost%3A3000%2Flogin&response_type=code");
	}, [])
	
	return (
	<div>
		Redirecting.. please wait
	</div>
	)
}

export default Login