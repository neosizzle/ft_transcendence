import React, { FunctionComponent } from "react";

interface EditUserNameProps {
	setCurrUsername : React.Dispatch<React.SetStateAction<string | null>>;
	currUsername : string | null;
}

const EditUsername:FunctionComponent<EditUserNameProps> = ({setCurrUsername, currUsername}) => {
	return <div>
		<label className="block mb-2 text-base font-medium text-gray-900 dark:text-gray-300">Username</label>
		<input className="bg-white appearance-none border-2 border-gray-200 rounded  py-2 px-4 text-gray-700 leading-tight"
		type="text"
		placeholder={currUsername || ""}
		value={currUsername || ""}
		onChange = {(e)=>setCurrUsername(e.target.value)}
		/>
	</div>
}

export default EditUsername