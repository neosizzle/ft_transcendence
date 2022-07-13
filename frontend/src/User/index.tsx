import React, { useEffect, useState } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { useAuth } from '../context/authContext';
import MenuIcon from './common/MenuIcon';
import UserDashboard from './UserDashboard';

const PROFILE_ACTIVE = 0;
const FRIENDS_ACTIVE = 1;
const BLOCKS_ACTIVE = 2;
const EDIT_ACTIVE = 3;
const SEARCH_ACTIVE = 4;


const UserIcon = () => {
	return <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
	<path strokeLinecap="round" strokeLinejoin="round" d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" />
</svg>
}

const FriendsIcon = () => {
	return <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
	<path strokeLinecap="round" strokeLinejoin="round" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" />
  </svg>
}

const BlocksIcon = () => {
	return <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
	<path strokeLinecap="round" strokeLinejoin="round" d="M18.364 18.364A9 9 0 005.636 5.636m12.728 12.728A9 9 0 015.636 5.636m12.728 12.728L5.636 5.636" />
  </svg>
}

const EditIcon = () => {
	return <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
	<path strokeLinecap="round" strokeLinejoin="round" d="M15.232 5.232l3.536 3.536m-2.036-5.036a2.5 2.5 0 113.536 3.536L6.5 21.036H3v-3.572L16.732 3.732z" />
  </svg>
}

const SearchIcon = () => {
	return <svg xmlns="http://www.w3.org/2000/svg" className="h-6 w-6" fill="none" viewBox="0 0 24 24" stroke="currentColor" strokeWidth={2}>
	<path strokeLinecap="round" strokeLinejoin="round" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
  </svg>
}

const User = () => {
	const auth = useAuth();
	const navigate = useNavigate();
	const location = useLocation();
	const [activeTab, setActiveTab] = useState<number>(PROFILE_ACTIVE);
	
	return (
		// bg-gradient-to-tl
		// from-slate-200 to-white
		<div
		className='
			sm:grid
			sm:grid-cols-12
			bg-[url("/assets/topography.svg")]
			bg-zinc-50
			min-h-screen
		'
		>
			{/**PC side menu */}
			<div
			className='
			hidden
			sm:block
			bg-slate-50
			sm:col-span-2
			lg:col-span-1
			h-full
			'
			>
				<div className="grid grid-rows-12 gap-0">
					<MenuIcon className = "col-span-2" caption='Profile' onClick={()=>navigate("/users/profile/me")} active = {location.pathname.endsWith("profile")}>
						<UserIcon/>
					</MenuIcon>
					<MenuIcon className = "col-span-2" caption='Friends' onClick={()=>navigate("/users/friends")} active = {location.pathname.endsWith("friends")}>
						<FriendsIcon/>
					</MenuIcon>
					<MenuIcon className = "col-span-2" caption='Blocks' onClick={()=>navigate("/users/blocks")} active = {location.pathname.endsWith("blocks")}>
						<BlocksIcon/>
					</MenuIcon>
					<MenuIcon className = "col-span-2" caption='Edit' onClick={()=>navigate("/users/edit")} active = {location.pathname.endsWith("edit")}>
						<EditIcon/>
					</MenuIcon>
					<MenuIcon className = "col-span-2" caption='Search' onClick={()=>navigate("/users/search")} active = {location.pathname.endsWith("search")}>
						<SearchIcon/>
					</MenuIcon>
				</div>
			</div>

			{
				typeof auth?.user === "string" ? 
				<h2>welcome {auth?.user}</h2>
				:
				<UserDashboard/>
			}

			{/**Mobile bottom menu */}
			<div
			className='
			block
			sm:hidden
			bg-slate-50
			fixed
			bottom-0
			left-0
			right-0
			'
			>
				<div className="grid grid-cols-5 gap-0">
					<MenuIcon caption='Profile' onClick={()=>setActiveTab(PROFILE_ACTIVE)} active = {activeTab === PROFILE_ACTIVE}>
						<UserIcon/>
					</MenuIcon>
					<MenuIcon caption='Friends' onClick={()=>setActiveTab(FRIENDS_ACTIVE)} active = {activeTab === FRIENDS_ACTIVE}>
						<FriendsIcon/>
					</MenuIcon>
					<MenuIcon caption='Blocks' onClick={()=>setActiveTab(BLOCKS_ACTIVE)} active = {activeTab === BLOCKS_ACTIVE}>
						<BlocksIcon/>
					</MenuIcon>
					<MenuIcon caption='Edit' onClick={()=>setActiveTab(EDIT_ACTIVE)} active = {activeTab === EDIT_ACTIVE}>
						<EditIcon/>
					</MenuIcon>
					<MenuIcon caption='Search' onClick={()=>setActiveTab(SEARCH_ACTIVE)} active = {activeTab === SEARCH_ACTIVE}>
						<SearchIcon/>
					</MenuIcon>
				</div>
			</div>

		</div>
	);
};

export default User;
