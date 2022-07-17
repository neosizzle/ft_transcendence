import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { auth_net_get } from "../../utils";
import Alert, { AlertType } from "../common/Alert";
import FriendCard from "../common/FriendCard";
import SearchBar from "../common/SearchBar";
import Pagination from "../common/Pagination";
import UnfriendModal from "../common/UnfriendModal";
import BlockCard from "../common/BlockCard";
import UnblockModal from "../common/UnblockModal";
import CardLoader from "../common/CardLoader";

const blocksEndpoint = `${API_ROOT}/blocks`;
const PAGE_SIZE = 5;

export interface Block {
	id?: number;
	updatedAt?: string;
	createdAt?: string;
	blocker?: User;
	blockerId?: number;
	blockee?: User;
	blockeeId?: number;
}

const Blocks: FunctionComponent = () => {
	const [blocks, setBlocks] = useState<Block[] | null>(null); // blocks list
	const [totalBlocks, setTotalBlocks] = useState<number>(-1); //total num of blocks
	const [currPage, setCurrPage] = useState<number>(1); //current page
	const [currRemovingUser, setCurrRemovingUser] = useState<User | null>(null); //current user to unblock

	const [openAlert, setOpenAlert] = useState<AlertType>({type: "", isOpen: false}); //open alert box
	const [alertMsg, setAlertMsg] = useState<string>(""); //alert message
	const [blocksSearchInput, setBlocksSearchInput] = useState<string>("");// friends search input
	const [loading, setLoading] = useState<boolean>(false); // loading state for search operation
	const navigate = useNavigate();
	const auth = useAuth();

	// get data every page change
	useEffect(() => {
		if (!currRemovingUser)
		{
			//get active blocks
			setLoading(true);
			setBlocks(null)
			auth_net_get(`${blocksEndpoint}?&page=${currPage}&pageSize=${PAGE_SIZE}&filterOn=username&filterBy=${blocksSearchInput}`)
			.then(data => {
				if (data.error && data.error == "Forbidden") return navigate("/logout");
				setBlocks(data.data)
				setTotalBlocks(data.total_elements);
				setLoading(false);
			})
		}
	}, [currPage, currRemovingUser, blocksSearchInput])
	
	return ( 
		<div>
			{/* Searchbar */}
			<SearchBar label = "Search by username" setSearchInput={setBlocksSearchInput} loading = {loading}/>

			{
				!blocks ? 
				<CardLoader/>
				:
				blocks.length < 1 ? 
				<div
				className="
				w-full
				flex
				justify-center
				"
				>
					<div className="w-10/12 mt-4 bg-white rounded-lg border border-gray-200 shadow-md p-10 flex flex-col justify-center items-center ">
						<img className="block h-20 sm:h-96 w-auto rounded" src="/assets/rock-sus.gif" alt="Workflow" />
						<div className="text-xl sm:text-2xl mt-6">You have no blocks.</div>
					</div>
				</div>
				:
				<div>


					{/* Friendslist */}
					{
						blocks.map((elem) => {
							return <BlockCard setCurrRemovingUser={setCurrRemovingUser} block={elem} currUser={auth?.user} key={elem.id}/>
						})
					}

					{/* Pagination */}
					{
						<Pagination setCurrPage={setCurrPage} currPage = {currPage} totalElements = {totalBlocks} pageSize={PAGE_SIZE}/>
					}

					{/* Unfriend confirmation */}
					{
						currRemovingUser ?
						<UnblockModal
						setAlertMsg={setAlertMsg}
						setOpenAlert={setOpenAlert}
						setCurrPage={setCurrPage}
						setCurrRemovingUser={setCurrRemovingUser}
						currRemovingUser = {currRemovingUser}/> : 
						null
					}

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
 
export default Blocks;