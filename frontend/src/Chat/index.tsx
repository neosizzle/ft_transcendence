import React, { useState, useEffect } from 'react';
import { API_ROOT } from '../constants';
import { useAuth } from '../context/authContext';
import { auth_net_get } from '../utils';
import { ERR, INCOMING_BAN, INCOMING_KICK, INCOMING_MSG } from "../constants";
import { Message, Room } from "./classes";

const chatEndpoint = `${API_ROOT}/chat`;
const roomEndpoint = `${API_ROOT}/rooms`;

function Chat() {
	const [currentMessage, setCurrentMessage] = useState('');
	const [messages, setMessages] = useState<Message[] | null>(null); // The chat
	const [rooms, setRooms] = useState<Room[] | null>(null); // total chat elemnts
	const auth = useAuth();

	const refreshPage = () => {
		auth_net_get(
			`${roomEndpoint}?page=1&pageSize=50`
		).then((data) => {
			const myRooms = data.data;
			const roomsArr: Room[] = [];
			myRooms.forEach((random: Room) => {
				roomsArr.push(random);
			});
			setRooms(roomsArr);
		})

		//For setting messages 
		auth_net_get(
			`${chatEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=1&sortBy=Ascending&sortOn=createdAt`
		).then((data) => {
			const chats = data.data;
			const msgsArr: Message[] = [];
			chats.forEach((chat: Message) => {
				msgsArr.push(chat);
			});
			setMessages(msgsArr);
		});
		console.log("Refreshing page");
	};

	const Test = () => {
		console.log("Hi I do nothing currently, am test");
	};

	useEffect(() => {
		auth_net_get(
			`${roomEndpoint}?page=1&pageSize=50`
		).then((data) => {
			const myRooms = data.data;
			const roomsArr: Room[] = [];
			myRooms.forEach((random: Room) => {
				roomsArr.push(random);
			});
			setRooms(roomsArr);
		})

		//For setting messages 
		auth_net_get(
			`${chatEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=1&sortBy=Ascending&sortOn=createdAt`
		).then((data) => {
			const chats = data.data;
			const msgsArr: Message[] = [];
			chats.forEach((chat: Message) => {
				msgsArr.push(chat);
			});
			setMessages(msgsArr);
		});
		if (!auth?.chatSocket) return;
		auth.chatSocket.on(INCOMING_MSG, refreshPage);
		auth.chatSocket.on(ERR, Test);
		auth.chatSocket.on(INCOMING_KICK, Test);
		auth.chatSocket.on(INCOMING_BAN, Test);

		return () => {
			// remove socket listeners
			auth?.chatWidgetSocket?.off(INCOMING_MSG, refreshPage);
			auth?.chatWidgetSocket?.off(ERR, Test);
			auth?.chatWidgetSocket?.off(INCOMING_KICK, Test);
			auth?.chatWidgetSocket?.off(INCOMING_BAN, Test);
		};
		//One problem I realised is, this is running very fast. Sometimes api crashes.
		// if (!auth?.chatSocket) return;
		// auth?.chatSocket.on("NewMessage", ClearInput);
		// auth?.chatSocket.on("message", ClearInput);

	}, [auth]);

	const sendMessage = async (event: React.FormEvent<HTMLFormElement>) => {
		event.preventDefault();
		if (currentMessage !== "") {
			const dto = {
				userId: auth?.user?.id,
				//How to get the current room?
				roomId: 1,
				message: currentMessage,
			};
			auth?.chatSocket?.emit("message", JSON.stringify(dto));
			ClearInput();
		}
	};
	const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
		setCurrentMessage(event.target.value);
	};
	const ClearInput = () => {
		// 👇️ clear input value
		setCurrentMessage('');
	};

	return (
		<>
			{/* Header */}
			<h1 className='text-5xl'>Chat</h1>
			<div className='grid grid-cols-3'>
				{/* <div className='flex flex-row'> */}
				{/* Need to figure out how to get user avatar. If DM, get user.avatar as src string. Else if GC, use default? (Alternatively
						can use owner picture) */}
				<div >{rooms?.map(room => <p className='border-2' key={room.id}>{room.roomName}</p>)}</div>
				<div className='h-96'>
					<div className='flex flex-row'>
						<p className='border-2'>Username</p> {/* Should be dynamic, based on the user you're messaging */}
						<p className='border-2'>View Profile</p>
						<p className='border-2'>Spectate</p>
						<p className='border-2'>Unfriend</p>
						<p className='border-2'>Block</p>
					</div>
					{messages?.map(message => <p key={message.id}>{message.message}</p>)}
					<form onSubmit={(sendMessage)} className="Chat">
						<input type="text" className="w-fit mt-10 border-2" placeholder="Say something..." id="message" onChange={handleChange} value={currentMessage} />
					</form>
					{/* </div> */}
				</div>
				<div>Members</div>
			</div>
		</>
	);
}

export default Chat;
