import React, { useState, useEffect } from 'react';
import { API_ROOT } from '../constants';
import { auth_net_get, auth_net_post } from '../utils';
import { Message } from "./classes";

const chatEndpoint = `${API_ROOT}/chat`;

function Chat() {
	const [currentMessage, setCurrentMessage] = useState('');
	// const [totalChats, setTotalChats] = useState(0); // total chat elemnts
	const [messages, setMessages] = useState<Message[] | null>(null); // The chat

	useEffect(() => {
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
			<div>
				<h1 className='text-5xl' id="MyHeader">Chat</h1>
			</div>
			<div className='h-96'>{messages?.map(message => <p key={message.id}>{message.message}</p>)}</div>
			<form onSubmit={(sendMessage)} className="Chat">
				<input type="text" className="w-fit mt-10 border-2" placeholder="Say something..." id="message" onChange={handleChange} value={currentMessage} />
			</form>
		</>
	);
}

export default Chat;
