import React, { FunctionComponent } from "react";
import { Route, Routes } from "react-router-dom";
import Friends from "./pages/Friends";
import Profile from "./pages/Profile";

const UserDashboard: FunctionComponent = () => {

	return ( 
		<div className="sm:col-span-10 lg:col-span-11">
			<Routes>
				<Route path = "profile/:id" element = {<Profile/>}/>
				<Route path = "friends" element = {<Friends/>}/>
				<Route path = "edit" element = {<div>EDIT_ACTIVE </div>}/>
				<Route path = "search" element = {<div>search_ACTIVE </div>}/>
				<Route path = "blocks" element = {<div>block_ACTIVE </div>}/>
			</Routes>	
		</div>
	);
}
 
export default UserDashboard;