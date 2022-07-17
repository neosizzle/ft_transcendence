import React, { FunctionComponent } from "react";
import { Route, Routes } from "react-router-dom";
import Blocks from "./pages/Blocks";
import Edit from "./pages/Edit";
import Friends from "./pages/Friends";
import Profile from "./pages/Profile";

const UserDashboard: FunctionComponent = () => {

	return ( 
		<div className="sm:col-span-10 lg:col-span-11">
			<Routes>
				<Route path = "profile/:id" element = {<Profile/>}/>
				<Route path = "friends" element = {<Friends/>}/>
				<Route path = "edit" element = {<Edit/>}/>
				<Route path = "search" element = {<div>search_ACTIVE </div>}/>
				<Route path = "blocks" element = {<div><Blocks/> </div>}/>
			</Routes>	
		</div>
	);
}
 
export default UserDashboard;