import React, { FormEvent, FunctionComponent, useState } from "react";
import { API_ROOT } from "../../constants";
import { auth_net_get } from "../../utils";
import { FriendShip } from "../pages/Friends";

const PAGE_SIZE = 5;
const friendsEndpoint = `${API_ROOT}/friends/?pageSize=${PAGE_SIZE}&page=1`

interface FriendsSearchBarProps {
	setFriends: React.Dispatch<React.SetStateAction<FriendShip[] | null >>;
}

const SearchLogo = () => {
	return <svg xmlns="http://www.w3.org/2000/svg" className="h-4 w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
	<path strokeLinecap="round" strokeLinejoin="round" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
  </svg>
}

const FriendsSearchBar: FunctionComponent<FriendsSearchBarProps> = ({setFriends}) => {
	const [input, setInput] = useState<string>("");
	const [loading, setLoading] = useState<boolean>(false);

	const handleSubmit = (e: FormEvent<HTMLFormElement>) => {
		e.preventDefault();
		setLoading(true)
		if (input === "")
		{
			auth_net_get(`${friendsEndpoint}`)
			.then(data => {
				setFriends(data.data)
				setLoading(false)
				//set total length
			})
		}
		else
		{
			auth_net_get(`${friendsEndpoint}&operator=OR&filterOn=intraName,username&filterBy=${input},${input}`)
			.then(data => {
				setFriends(data.data)
				setLoading(false)
				//set total length
			})
		}
		setInput("");
	}

	return ( 
		<div
		className="
		flex
		justify-center
		w-full
		mt-2
		"
		>
			<div className="
			w-4/5
			">
				<form className="flex items-center" onSubmit={(e) => handleSubmit(e)}>   
					<label className="sr-only">Search</label>
					<div className="relative w-full">
						<div className="flex absolute inset-y-0 left-0 items-center pl-3 pointer-events-none">
							<svg className="w-5 h-5 text-gray-500 dark:text-gray-400" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg"><path d="M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z" ></path></svg>
						</div>
						<input type="text" value = {input} onChange = {(e)=>setInput(e.target.value)} className="bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-gray-500 focus:border-gray-500 block w-full pl-10 p-2.5  dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-gray-500 dark:focus:border-gray-500" placeholder="Search"/>
					</div>
					<button disabled={loading} type="submit" className="p-2.5 ml-2 text-sm font-medium text-white bg-gray-700 rounded-lg border border-gray-700 hover:bg-gray-800 focus:ring-4 focus:outline-none focus:ring-gray-300 dark:bg-gray-600 dark:hover:bg-gray-700 dark:focus:ring-gray-800">
						<SearchLogo/>
					</button>
				</form>
			</div>
		</div>
	);
}
 
export default FriendsSearchBar;