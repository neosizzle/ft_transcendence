import React, { FunctionComponent, useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { User } from "../../../../context/authContext";
import { Block } from "../Blocks";
import { IngameBadge, OfflineBadge, OnlineBadge } from "../../../common/Badges";

interface BlockCardProps {
	block?: Block;
	currUser?: User | null;
	key?: number;
	setCurrRemovingUser: React.Dispatch<React.SetStateAction<User | null >>;
}

const BlockCard: FunctionComponent<BlockCardProps> = ({block, currUser, setCurrRemovingUser}) => {
	const [userToDisplay, setUserToDisplay] = useState<User | null>(null);

	useEffect(() => {
		if (block?.blockee)
			setUserToDisplay(block.blockee);
	}, [])
	
	return ( 
		<div className =
		"w-full flex justify-center mt-2">
			<div className="w-10/12 bg-white rounded-lg border border-gray-200 shadow-md p-3">
				<div className="grid grid-cols-12 gap-4">

					{/* Avatar */}
					<div className="col-span-3 flex justify-center items-center">
						<img className="
							inline-block
							h-10
							w-10
							sm:h-16
							sm:w-16
							rounded-full
							ring-1
							ring-white
							mt-3
							mb-1
							" src={userToDisplay?.avatar || '/assets/default-pp.webp'} alt=""/>
					</div>

					{/* user info */}
					<div className="col-span-6">
						<div>
							<Link to={`/users/profile/${userToDisplay?.id}`}><span className="font-medium">{userToDisplay?.username}</span> <span className="text-xs font-light">{userToDisplay?.intraName}</span></Link>
						</div>

						<div>
							{userToDisplay?.status === "LOGGEDIN" ? <OnlineBadge/> : userToDisplay?.status === "OFFLINE" ? <OfflineBadge/> : userToDisplay?.status === "INGAME" ? <IngameBadge/> : null}
						</div>

						<div className="hidden sm:block">
							Level {userToDisplay?.level}
						</div>
					</div>

					{/* Action buttons */}
					<div className="col-span-3 h-full flex justify-end items-end">

					<button onClick={()=>setCurrRemovingUser(userToDisplay)}
					type="button" className="text-white bg-red-700 hover:bg-red-800 focus:ring-4 focus:outline-none focus:ring-red-300 font-medium rounded-full text-sm p-2 text-center inline-flex items-center mr-2">
						<svg xmlns="http://www.w3.org/2000/svg" className="h-3 w-3 sm:h-4 sm:w-4" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
							<path strokeLinecap="round" strokeLinejoin="round" d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z" />
						</svg>
					</button>
					</div>

				</div>
			</div>
		</div>
	);
}
 
export default BlockCard;