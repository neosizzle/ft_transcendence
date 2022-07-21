import React from 'react'
import { Navigate } from 'react-router-dom';
import { useAuth } from '../context/authContext'

interface Props {
	children : React.ReactNode
}

const Protected = (props : Props) => {
	const auth = useAuth();

	if (!auth?.user)
		return <Navigate to = "/login"/>
	return <>{props.children}</>
}

export default Protected