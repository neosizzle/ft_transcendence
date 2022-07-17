import React, { FunctionComponent, useState } from "react";
import { useAuth } from "../../context/authContext";
import EditAvatar from "../common/EditAvatar";

const Edit: FunctionComponent = () => {
	const auth = useAuth();
	const [currAvatar, setCurrAvatar] = useState<string>(auth?.user?.avatar ? auth?.user?.avatar : '/assets/default-pp.webp');

	return (
	<div>
		<EditAvatar currAvatar={currAvatar} setCurrAvatar={setCurrAvatar}/>
	</div> );
}
 
export default Edit;