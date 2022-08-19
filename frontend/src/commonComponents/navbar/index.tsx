import React, { useState } from "react";
import { Link, useLocation } from "react-router-dom";
import { useAuth } from "../../context/authContext";
import SideDialog from "./SideDialog";

const Navbar = () => {
  const auth = useAuth();
  const location = useLocation();
  const [openMenu, setOpenMenu] = useState<boolean>(false);

  return (
    // Hide navbar when login or logout
    location.pathname === "/login" || location.pathname === "/logout" ? null : (
      <nav className="bg-black">
        <div className="max-w-7xl mx-auto px-2 sm:px-6 lg:px-8">
          <div className="relative flex items-center justify-between h-16">
            {/* Icon left */}
            <div className="flex-shrink-0 flex items-center">
              <Link to="/" className="" aria-current="page">
                <img
                  className="block h-10 w-auto"
                  src="/assets/logo/42logo.png"
                  alt="42logo"
                />
              </Link>
            </div>

            {/* Nav links */}
            <div className="hidden sm:block sm:ml-6">
              <div className="flex space-x-4">
                {/* <Link to="/" className="bg-gray-900 text-white px-3 py-2 rounded-md text-sm font-medium" aria-current="page">Home</Link> */}
                {auth?.user ? (
                  <Link
                    to="/logout"
                    className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
                  >
                    Logout
                  </Link>
                ) : (
                  <Link
                    to="/login"
                    className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
                  >
                    Login
                  </Link>
                )}
                <Link
                  to="/chat"
                  className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
                >
                  Chat
                </Link>
                <Link
                  to="/users/profile/me"
                  className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
                >
                  Users
                </Link>
                <Link
                  to="/game"
                  className="text-gray-300 hover:bg-gray-700 hover:text-white px-3 py-2 rounded-md text-sm font-medium"
                >
                  Game
                </Link>
              </div>
            </div>

            {/* Menu open button */}
            <div className="block ml-6 sm:hidden">
              <button
                onClick={() => setOpenMenu(true)}
                className="
                  bg-transparent
                  text-white
                  hover:bg-white-500
                  text-white-700
                  font-semibold
                  hover:text-white
                  py-2
                  px-4 
                  border
                  border-white-500
                  hover:border-transparent
                  rounded"
              >
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  className="h-6 w-6"
                  fill="none"
                  viewBox="0 0 24 24"
                  stroke="currentColor"
                  strokeWidth={2}
                >
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    d="M4 6h16M4 12h16M4 18h16"
                  />
                </svg>
              </button>
            </div>
          </div>
        </div>

        {/* Left menu */}
        {openMenu ? <SideDialog setOpenMenu={setOpenMenu} /> : null}
      </nav>
    )
  );
};

export default Navbar;
