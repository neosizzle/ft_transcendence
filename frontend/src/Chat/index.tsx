import React, { useState, useEffect } from 'react';
import { API_ROOT } from '../constants';
import { auth_net_get, auth_net_post } from '../utils';
import { Message, Room } from "./classes";

const chatEndpoint = `${API_ROOT}/chat`;
const roomEndpoint = `${API_ROOT}/rooms`;

function Chat() {
	const [currentMessage, setCurrentMessage] = useState('');
	const [messages, setMessages] = useState<Message[] | null>(null); // The chat
	const [rooms, setRooms] = useState<Room[] | null>(null); // total chat elemnts

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
		console.log("Wow this is running a lot");
		//One problem I realised is, this is running very fast. Sometimes api crashes.
	});

	const sendMessage = async (event: React.FormEvent<HTMLFormElement>) => {
		event.preventDefault();
		if (currentMessage !== "") {
			const dto = {
				//How to get the current user?
				userId: 1,
				roomId: 1,
				message: `${currentMessage}`,
			};
			auth_net_post(
				`${chatEndpoint}`, dto
			)
				.then((data) => console.log(data))
				.catch(e => console.log(e))
			console.log("Hi");
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
			<div className='grid grid-cols-2'>
				<div className='flex flex-row'>
					{/* Need to figure out how to get user avatar. If DM, get user.avatar as src string. Else if GC, use default? (Alternatively
						can use owner picture) */}
					<div className='basis-1/4'>{rooms?.map(room => <p className='border-2' key={room.id}>{room.roomName}</p>)}</div>
					<div className='h-96'>{messages?.map(message => <p key={message.id}>{message.message}</p>)}</div>
				</div>
			</div>
			<form onSubmit={(sendMessage)} className="Chat">
				<input type="text" className="w-fit mt-10 border-2" placeholder="Say something..." id="message" onChange={handleChange} value={currentMessage} />
			</form>
		</>
	);
}

export default Chat;
