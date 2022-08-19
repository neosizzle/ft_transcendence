import React, { FunctionComponent, useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { User } from "../../../../context/authContext";
import { FriendShip } from "../Friends";
import { IngameBadge, OfflineBadge, OnlineBadge } from "../../../common/Badges";

interface FriendCardProps {
  friendship?: FriendShip;
  currUser?: User | null;
  key?: number;
  setCurrRemovingUser: React.Dispatch<React.SetStateAction<User | null>>;
}

const FriendCard: FunctionComponent<FriendCardProps> = ({
  friendship,
  currUser,
  setCurrRemovingUser,
}) => {
  const [userToDisplay, setUserToDisplay] = useState<User | null>(null);

  useEffect(() => {
    if (friendship?.userId === currUser?.id)
      setUserToDisplay(friendship?.friend || null);
    else setUserToDisplay(friendship?.user || null);
  }, []);

  return (
    <div className="w-full flex justify-center mt-2">
      <div className="w-10/12 bg-white rounded-lg border border-gray-200 shadow-md p-3">
        <div className="grid grid-cols-12 gap-4">
          {/* Avatar */}
          <div className="col-span-3 flex justify-center items-center">
            <img
              className="
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
							"
              src={userToDisplay?.avatar || "/assets/default-pp.webp"}
              alt=""
            />
          </div>

          {/* user info */}
          <div className="col-span-6">
            <div>
              <Link to={`/users/profile/${userToDisplay?.id}`}>
                <span className="font-medium">{userToDisplay?.username}</span>{" "}
                <span className="text-xs font-light">
                  {userToDisplay?.intraName}
                </span>
              </Link>
            </div>

            <div>
              {userToDisplay?.status === "LOGGEDIN" ? (
                <OnlineBadge />
              ) : userToDisplay?.status === "OFFLINE" ? (
                <OfflineBadge />
              ) : userToDisplay?.status === "INGAME" ? (
                <IngameBadge />
              ) : null}
            </div>

            <div className="hidden sm:block">Level {userToDisplay?.level}</div>
          </div>

          {/* Action buttons */}
          <div className="col-span-3 h-full flex justify-end items-end">
            <button
              type="button"
              className="text-white bg-gray-700 hover:bg-gray-800 focus:ring-4 focus:outline-none focus:ring-gray-300 font-medium rounded-full text-sm p-2 text-center inline-flex items-center mr-2"
            >
              <svg
                xmlns="http://www.w3.org/2000/svg"
                className="h-3 w-3 sm:h-4 sm:w-4"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                strokeWidth={2}
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z"
                />
              </svg>
            </button>

            <button
              onClick={() => setCurrRemovingUser(userToDisplay)}
              type="button"
              className="text-white bg-red-700 hover:bg-red-800 focus:ring-4 focus:outline-none focus:ring-red-300 font-medium rounded-full text-sm p-2 text-center inline-flex items-center mr-2"
            >
              <svg
                xmlns="http://www.w3.org/2000/svg"
                className="h-3 w-3 sm:h-4 sm:w-4"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
                strokeWidth={2}
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"
                />
              </svg>
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default FriendCard;
