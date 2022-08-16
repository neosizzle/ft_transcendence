import React, { FunctionComponent } from "react";
import { Route, Routes } from "react-router-dom";
import Blocks from "./pages/blocks/Blocks";
import Edit from "./pages/edit/Edit";
import Friends from "./pages/friends/Friends";
import Profile from "./pages/profile/Profile";
import Search from "./pages/search/Search";

const UserDashboard: FunctionComponent = () => {
  return (
    <div className="sm:col-span-10 lg:col-span-11">
      <Routes>
        <Route path="profile/:id" element={<Profile />} />
        <Route path="friends" element={<Friends />} />
        <Route path="edit" element={<Edit />} />
        <Route path="search" element={<Search />} />
        <Route
          path="blocks"
          element={
            <div>
              <Blocks />{" "}
            </div>
          }
        />
      </Routes>
    </div>
  );
};

export default UserDashboard;
