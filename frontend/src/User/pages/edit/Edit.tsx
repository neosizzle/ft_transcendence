import React, { FunctionComponent, useState } from "react";
import { useAuth } from "../../../context/authContext";
import EditAvatar from "./components/EditAvatar";
import EditEmail from "./components/EditEmail";
import EditUsername from "./components/EditUsername";

const Edit: FunctionComponent = () => {
	const auth = useAuth();
	const [currAvatar, setCurrAvatar] = useState<string>(auth?.user?.avatar ? auth?.user?.avatar : '/assets/default-pp.webp');
	const [currUsername, setCurrUsername] = useState<string | null>(auth?.user?.username ? auth.user.username : null);
	const [currEmail, setCurrEmail] = useState<string | null>(auth?.user?.email ? auth.user.email : null);

	return (
	<div>
		<EditAvatar currAvatar={currAvatar} setCurrAvatar={setCurrAvatar}/>
		<EditUsername setCurrUsername={setCurrUsername} currUsername={currUsername}/>
		<EditEmail setCurrEmail={setCurrEmail} currEmail={currEmail}/>

		<button type="button" className={`text-white bg-green-600 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 dark:focus:ring-green-800 font-medium rounded-lg text-sm inline-flex items-center px-5 py-2.5 text-center mr-2`}>
					Save changes
				</button>
	</div> );
}
 
export default Edit;