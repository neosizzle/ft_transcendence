import React, { FunctionComponent, useEffect, useState } from "react";
import { useAuth } from "../context/authContext";
import { SocketInterface } from "./classes";

const handleNewMsg = (data : any) => {
	alert(data.message)
}

const WsRoot : FunctionComponent = () => {
	
	const socket = new SocketInterface(handleNewMsg);
	const auth = useAuth();

	useEffect(() => {

		return () => {
			socket.destroy()
		}
	}, [])

	return ( 
		<div>
				<button className="px-3 py-1 border border-gray-300" onClick={()=>socket?.socket?.disconnect()}>
					disconnect
				</button>

				<button className="px-3 py-1 border border-gray-300" onClick={()=>socket?.socket?.emit('create', JSON.stringify({type  : "gc", userId : auth?.user?.id, initialUsers : ["60", "121"]}))}>
					create room gc
				</button>
		</div>
	);
}
 

const WsTest: FunctionComponent = () => {

	return ( 
		<WsRoot/>
	);
}
 
export default WsTest;