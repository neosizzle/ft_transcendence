import React, { FunctionComponent } from "react";

interface EditEmailProps {
	setCurrEmail : React.Dispatch<React.SetStateAction<string | null>>;
	currEmail : string | null;
}

const EditEmail:FunctionComponent<EditEmailProps> = ({setCurrEmail, currEmail}) => {
	return <div>
		<label className="block mb-2 text-base font-medium text-gray-900 dark:text-gray-300">Email</label>
		<input disabled className="bg-gray-200 appearance-none border-2 border-gray-200 rounded  py-2 px-4 text-gray-700 leading-tight"
		type="text"
		placeholder={currEmail || ""}
		value={currEmail || ""}
		/>
		<button onClick={()=> alert("2FA HERE")}>
			Change email
		</button>
	</div>
}

export default EditEmail