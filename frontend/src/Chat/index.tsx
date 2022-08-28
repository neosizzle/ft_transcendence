import React, { useState, useEffect, useRef } from 'react';
import { API_ROOT } from '../constants';
import { useAuth, User } from '../context/authContext';
import { auth_net_get } from '../utils';
import { ERR, INCOMING_BAN, INCOMING_KICK, INCOMING_MSG } from "../constants";
import { Member, Message, Room } from "./classes";
import { NavLink } from 'react-router-dom';

const roomEndpoint = `${API_ROOT}/rooms`;
const chatEndpoint = `${API_ROOT}/chat`;
const memberEndpoint = `${API_ROOT}/members`;

function Chat() {
	const auth = useAuth();
	const [rooms, setRooms] = useState<Room[] | null>(null); // total chat elemnts
	const [activeRoom, setActiveRoom] = useState<Room | null>(null);
	const activeRoomRef = useRef(activeRoom);
	const setActiveRoomRef = (data: Room) => activeRoomRef.current = data;
	const [currentMessage, setCurrentMessage] = useState('');
	const [messages, setMessages] = useState<Message[] | null>(null); // The chat
	const [memberUsers, setMemberUsers] = useState<User[] | null>(null);

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
		console.log("state ", activeRoom?.id);
		console.log("ref ", activeRoomRef.current?.id);
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

	useEffect(() => {
		//For setting messages 
		if (rooms != null) {
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

	useEffect(() => {
		//For setting messages 
		if (activeRoom != null) {
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
			auth_net_get(
				`${memberEndpoint}?page=1&pageSize=50&filterOn=roomId&filterBy=${activeRoom.id}`
			).then((data) => {
				const member = data.data;
				const userArr: User[] = [];
				member.forEach((member: Member) => {
					userArr.push(member.user);
				});
				setMemberUsers(userArr);
			});
		}
	}, [activeRoom]);

	const sendMessage = async (event: React.FormEvent<HTMLFormElement>) => {
		event.preventDefault();
		if (currentMessage !== "") {
			const dto = {
				userId: auth?.user?.id,
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
				{/* Need to figure out how to get user avatar. If DM, get user.avatar as src string. Else if GC, use default? (Alternatively
						can use owner picture) */}
				<div>{rooms?.map(room => <div className='text-xl border-2 cursor-pointer'
					onClick={() => {
						setActiveRoom(room);
						setActiveRoomRef(room);
					}}
					key={room.id}>
					{
						room.type === "DM" ? <img src="/assets/default-pp.webp" width="100" height="100"/> : <img src="/assets/default-gc-harambe.png" width="100" height="100" /> // Can't pass in value to src for some reason?
					}
					{
						room.type === "DM" ? "Is a DM (Should display other user's name)" : room.roomName // But how to get specific room's users
					}
				</div>)}</div>
				<div className='h-96'>
					<div className='flex flex-row'>
						{
							activeRoom?.type === "DM" && memberUsers != null ? auth?.user?.id === memberUsers[0].id ? <p className='text-lg border-2'>{memberUsers[1].username} </p> : <p className='text-lg border-2'>{memberUsers[0].username} </p> : <p className='text-lg border-2'>{activeRoom?.roomName}</p>
						}
						<NavLink to="/users/profile/1" className='border-2'>View Profile</NavLink>
						<p className='border-2'>Spectate</p>
						<p className='border-2'>Unfriend</p>
						<p className='border-2'>Block</p>
					</div>
					{messages?.map(message =>
						<div key={message.id}>
							{
								message.userId === auth?.user?.id ? <div><div className="text-lg text-right">{message.message}</div>
									<div className='text-xs text-grey text-right'>{new Date(message.createdAt).toLocaleString("en-gb", { hour12: true })}</div></div>
									: <div><div className="text-lg">{message.message}</div><div className='text-xs text-grey'>{new Date(message.createdAt).toLocaleString("en-gb", { hour12: true })}</div></div>
							}
						</div>
					)}
					<form onSubmit={(sendMessage)} className="Chat">
						<input type="text" className="w-fit mt-10 border-2" placeholder="Say something..." id="message" onChange={handleChange} value={currentMessage} />
					</form>
					{/* </div> */}
				</div>
				<div>
					Members
					<div>
						{memberUsers?.map(user => <div key={user.id}>{`${user.username}`}</div>)}
					</div>
				</div>
			</div>
		</>
	);
}

export default Chat;
