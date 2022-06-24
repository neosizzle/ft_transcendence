import React from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../context/authContext';

const User = () => {
	const auth = useAuth();
	const navigate = useNavigate()

	const handleLogout = () => {
		auth?.logout();
		navigate("/");
	}

	return (
		<div>
			<h2>welcome {auth?.user}</h2>
			<button onClick={handleLogout}>
				logout
			</button>
		</div>
	);
};

export default User;
