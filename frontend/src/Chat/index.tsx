import React from 'react';

function Chat() {
	let messages = "Test";

	function myFunction(event: React.FormEvent<HTMLFormElement>) {
		event.preventDefault();
		messages = "Boom";
		alert("The form was submitted");
	}

	return (
		<>
			<div>
				<h1 className='text-5xl'>Chat</h1>
			</div>
			{messages}
			<form onSubmit={(myFunction)} className="Chat">
				<input className="w-fit mt-10 border-2" placeholder="Say something..." />
			</form>
		</>
	);
}

export default Chat;
