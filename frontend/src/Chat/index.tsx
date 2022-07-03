import React from 'react';
import "./index.css";

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
			<h2>chat</h2>
		</div>
		{messages}
		<form onSubmit={(myFunction)} className="Chat">
			<input/>
			<button type="submit">Submit</button>
		</form>
		</>
	);
}

export default Chat;
