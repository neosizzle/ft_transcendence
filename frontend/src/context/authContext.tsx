import { createContext, useContext, useState } from "react";
import React from 'react';
import { APT_ROOT, TOKEN_KEY } from "../constants";
import { auth_net_get, net_get } from "../utils";

// TODO HANDLE ERRORS

const authEndpoint = `${APT_ROOT}/auth/authenticate`
const userEndpoint = `${APT_ROOT}/users/me`

interface Props {
    children : React.ReactNode
}

interface User {
	id : number,
	email : string | null,
	avatar : string | null,
	status : string,
	level : number,
	username : string,
	intraID : string,
	intraName : string,
	createdAt : string,
	updatedAt : string
}

interface AuthCtx {
    user : User | string | null,
	login: (code : string | null) => void ,
	logout: () => void
}

const AuthContext = createContext<AuthCtx | null>(null);

export const AuthProvider = (props : Props) => {
	const [user, setUser] = useState<User | string | null>(null);

	const login = (code : string | null) => {
		// setUser(code)
		if (!code)
		{
			auth_net_get(userEndpoint)
			.then(res => setUser(res))
		}
		net_get(`${authEndpoint}/?code=${code}`)
		.then(async data => {
			localStorage.setItem(TOKEN_KEY, data.data.token)
			const res = await auth_net_get(userEndpoint)
			setUser(res)
		})
	}
	const logout = () => {
		setUser(null)
		localStorage.removeItem(TOKEN_KEY)
	}

	return (
		<AuthContext.Provider value = {{user, login, logout}}>
			{props.children}
		</AuthContext.Provider>
	)
}

export const useAuth = () => {
	return useContext(AuthContext);
};