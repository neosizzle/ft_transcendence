import React, { useState, useEffect, useRef } from 'react';
import { API_ROOT } from '../constants';
import { useAuth } from '../context/authContext';
import { auth_net_get } from '../utils';
import { ERR, INCOMING_BAN, INCOMING_KICK, INCOMING_MSG } from "../constants";
import { Message, Room } from "./classes";
import { NavLink } from 'react-router-dom';

const chatEndpoint = `${API_ROOT}/chat`;
const roomEndpoint = `${API_ROOT}/rooms`;

function Chat() {
	const [currentMessage, setCurrentMessage] = useState('');
	const [messages, setMessages] = useState<Message[] | null>(null); // The chat
	const [rooms, setRooms] = useState<Room[] | null>(null); // total chat elemnts
	const [activeRoom, setActiveRoom] = useState<Room | null>(null);
	const activeRoomRef = useRef(activeRoom);
	const setActiveRoomRef = (data : Room) => activeRoomRef.current = data;
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
		console.log("state " , activeRoom?.id);
		console.log("ref " , activeRoomRef.current?.id);
		//Also doesn't work?
	};

	const Test = () => {
		console.log("Hi I do nothing currently, am test");
		console.log(activeRoom?.id);
	};

	useEffect(() => {
		auth_net_get(
			`${roomEndpoint}?page=1&pageSize=50`
		).then((data) => {
			const myRooms = data.data;
			const roomsArr: Room[] = [];
			myRooms.forEach((room: Room) => {
				roomsArr.push(room);
			});
			setRooms(roomsArr);
		})
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

	useEffect(()=>{
	//For setting messages 
	if (rooms != null)
		{
			setActiveRoom(rooms[0]);
			setActiveRoomRef(rooms[0]);
			auth_net_get(
				`${chatEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=${rooms[0].id}&sortBy=Ascending&sortOn=createdAt`
			).then((data) => {
				const chats = data.data;
				const msgsArr: Message[] = [];
				chats.forEach((chat: Message) => {
					msgsArr.push(chat);
				});
				setMessages(msgsArr);
			});
		}
	}, [rooms]);

	useEffect(()=>{
		//For setting messages 
		if (activeRoom != null)
			{
				auth_net_get(
					`${chatEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=${activeRoom.id}&sortBy=Ascending&sortOn=createdAt`
				).then((data) => {
					const chats = data.data;
					const msgsArr: Message[] = [];
					chats.forEach((chat: Message) => {
						msgsArr.push(chat);
					});
					setMessages(msgsArr);
				});
			}
		}, [activeRoom]);

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
				<div>{rooms?.map(room => <div className='text-xl border-2 cursor-pointer' 
				// If dm, display name instead of roomname
				onClick={() => {
					setActiveRoom(room);
					setActiveRoomRef(room);
				}}
				key={room.id}>{room.roomName}</div>)}</div>
				<div className='h-96'>
					<div className='flex flex-row'>
						<p className='text-lg border-2'>{activeRoom?.roomName}</p> {/* Should be dynamic, based on the user you're messaging */}
						<NavLink to="/users/profile/1" className='border-2'>View Profile</NavLink>
						{/* <a href="users/profile/1" className='border-2'>View Profile</a> Problem is this doesn't work cause just links back to start */}
						<p className='border-2'>Spectate</p>
						<p className='border-2'>Unfriend</p>
						<p className='border-2'>Block</p>
					</div>
					{messages?.map(message => <div key={message.id}>{`${message.message}`} <div className='text-sm text-grey'>{new Date(message.createdAt).toString()}</div></div>)}
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
