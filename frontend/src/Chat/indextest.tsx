import React, { useState } from 'react';

function Chat() {
	const [currentMessage, setCurrentMessage] = useState('');

	const sendMessage = async (event: React.FormEvent<HTMLFormElement>) => {
		event.preventDefault();
		if (currentMessage !== "") {
			const jsonData = {
				"UserID": 123,
				"timestamp": "2022-04-23T18:25:43.511Z",
				"message": "Omg it worked"
			}
			fetch("http://localhost:3001/chat"), {
				method : 'POST',
				mode: 'cors',
				body: JSON.stringify(jsonData)
			}
			// const messageData = {
			// 	userID: 5,
			// 	message: currentMessage,
			// 	timestamp: new Date(Date.now())
			// };
			// try {
			// 	const res = await postMessage("/chat", messageData);
			// } catch (err) {
			// 	console.log(err);
			// }
			console.log("Hi");
		}
	};
	const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
		setCurrentMessage(event.target.value);
	};
	// const ClearInput = () => {
		// 👇️ clear input value
		// setCurrentMessage('');
	// };
	// function sendMessage(event: React.FormEvent<HTMLFormElement>) {
	// 	event.preventDefault();
	// 	const element = document.getElementById("chat");
	// 	const message = (document.getElementById("message") as HTMLInputElement).value;
	// 	if (element && message) {
	// 		element.innerHTML += message;
	// 		element.innerHTML += '<br>';
	// 	}
	// 	ClearInput();
	// }

	return (
		<>
			<div>
				<h1 className='text-5xl' id="MyHeader">Chat</h1>
			</div>
			<p id="chat" className='h-96'></p>
			<form onSubmit={(sendMessage)} className="Chat">
				<input type="text" className="w-fit mt-10 border-2" placeholder="Say something..." id="message" onChange={handleChange} value={currentMessage}/>
			</form>
		</>
	);
}

export default Chat;