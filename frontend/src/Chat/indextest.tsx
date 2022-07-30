import { useState } from 'react';
import React from 'react';

function Chat() {
	const [message, setMessage] = useState('');

	const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
		console.log(setMessage);
		setMessage(event.target.value);
	};
	const handleClick = () => {
		// 👇️ clear input value
		setMessage('');
	};
	function myFunction(event: React.FormEvent<HTMLFormElement>) {
		event.preventDefault();
		const element = document.getElementById("chat");
		const message = (document.getElementById("message") as HTMLInputElement).value;
		if (element && message) {
			element.innerHTML += message;
			element.innerHTML += '<br>';
		}
		handleClick();
	}

	return (
		<>
			<div>
				<h1 className='text-5xl' id="MyHeader">Chat</h1>
			</div>
			<p id="chat" className='h-96'></p>
			<form onSubmit={(myFunction)} className="Chat">
				<input type="text" className="w-fit mt-10 border-2" placeholder="Say something..." id="message" onChange={handleChange} value={message}/>
			</form>
		</>
	);
}

export default Chat;
