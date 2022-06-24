import { createContext, useContext, useState } from "react";
import React from 'react';

interface Props {
    children : React.ReactNode
}

interface AuthCtx {
    user : string | null,
	login: (userNew : string) => void ,
	logout: () => void
}

const AuthContext = createContext<AuthCtx | null>(null);

export const AuthProvider = (props : Props) => {
	const [user, setUser] = useState<string | null>(null);

	const login = (userNew : string) => {setUser(userNew)}
	const logout = () => {setUser(null)}

	return (
		<AuthContext.Provider value = {{user, login, logout}}>
			{props.children}
		</AuthContext.Provider>
	)
}

export const useAuth = () => {
	return useContext(AuthContext);
};