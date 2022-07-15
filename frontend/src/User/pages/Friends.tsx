import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { auth_net_get } from "../../utils";
import Alert, { AlertType } from "../common/Alert";
import FriendCard from "../common/FriendCard";
import FriendsSearchBar from "../common/FriendsSearchBar";
import Pagination from "../common/Pagination";
import UnfriendModal from "../common/UnfriendModal";

const friendsEndpoint = `${API_ROOT}/friends`
const PAGE_SIZE = 1;

export interface FriendShip {
	id?: number;
	updatedAt?: string;
	createdAt?: string;
	friend?: User;
	friendId?: number;
	user?: User;
	userId?: number;
	reqStatus?: string;
}

const Friends: FunctionComponent = () => {
	const [friends, setFriends] = useState<FriendShip[] | null>(null); //friends list
	const [totalFriends, setTotalFriends] = useState<number>(-1); //total num of friends
	const [pendingFriends, setPendingFriends] = useState<FriendShip[] | null>(null); //pending friends list
	const [totalPendingFriends, setTotalPendingFriends] = useState<number>(-1); //total num of pending friends
	const [currPage, setCurrPage] = useState<number>(1); //current page
	const [currPendingPage, setCurrPendingPage] = useState<number>(1);  //current pending page
	const [currRemovingUser, setCurrRemovingUser] = useState<User | null>(null); //current user to unfriend
	const [openAlert, setOpenAlert] = useState<AlertType>({type: "", isOpen: false}); //open alert box
	const [alertMsg, setAlertMsg] = useState<string>(""); //alert message
	const navigate = useNavigate();
	const auth = useAuth();

	// get data every page change
	useEffect(() => {
		if (!currRemovingUser)
		{
			//get active friends 
			auth_net_get(`${friendsEndpoint}?page=${currPage}&pageSize=${PAGE_SIZE}&filterOn=reqStatus&filterBy=APPROVED`)
			.then(data => {
				if (data.error && data.error == "Forbidden") return navigate("/logout");
				// console.log(data)
				setFriends(data.data)
				setTotalFriends(data.total_elements)
			})
		}
	}, [currPage, currRemovingUser])
	
	// get data every pending friends page change
	useEffect(() => {
		
		// get pending friends
		auth_net_get(`${friendsEndpoint}?page=${currPage}&pageSize=${PAGE_SIZE}&filterOn=reqStatus&filterBy=PENDING`)
		.then(data => {
			if (data.error && data.error == "Forbidden") return navigate("/logout");
			// console.log(data)
			setPendingFriends(data.data)
			setTotalPendingFriends(data.total_elements)
		})
			
	}, [currPendingPage])
	

	return ( 
		<div>
			{/* Searchbar */}
			<FriendsSearchBar setFriends={setFriends}/>

			{
				!friends ? 
				<div>getting data...</div>
				:
				friends.length < 1 ? 
				<div>no friends..</div>
				:
				<div>


					{/* Friendslist */}
					{
						friends.map((elem) => {
							return <FriendCard setCurrRemovingUser={setCurrRemovingUser} friendship={elem} currUser={auth?.user} key={elem.id}/>
						})
					}

					{/* Pagination */}
					{
						<Pagination setCurrPage={setCurrPage} currPage = {currPage} totalElements = {totalFriends} pageSize={PAGE_SIZE}/>
					}

					{/* Unfriend confirmation */}
					{
						currRemovingUser ?
						<UnfriendModal
						setAlertMsg={setAlertMsg}
						setOpenAlert={setOpenAlert}
						setCurrPage={setCurrPage}
						setCurrRemovingUser={setCurrRemovingUser}
						currRemovingUser = {currRemovingUser}/> : 
						null
					}

					{/* pending Friendslist */}
					

					{/* Alert feedback */}
					{
						openAlert.isOpen ? 
						<Alert alert = {openAlert} setOpenAlert={setOpenAlert} message={alertMsg}/> : 
						null
					}
				</div>
			}
		</div>
	);
}
 
export default Friends;