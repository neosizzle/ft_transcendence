import React, { useEffect, useState } from 'react'
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../context/authContext';

const Logout = (props: any) => {
	const auth = useAuth();
	const navigate = useNavigate();

	const handleLogout = async () =>
	{
		await auth?.logout()
		navigate("/", {replace : true})
	}

	useEffect(() => {
		handleLogout();
	}, [])
	
	return (
	<div>
		Redirecting.. please wait
	</div>
	)
}

export default Logout