import React, { FunctionComponent } from "react";
import { Link } from "react-router-dom";
import { AlertType } from "../../../../commonComponents/Alert";
import { User } from "../../../../context/authContext";
import useWindowDimensions from "../../../../hooks/useWindowDimensions";
import { IngameBadge, OfflineBadge, OnlineBadge } from "../../../common/Badges";
import ActionMenu from "./ActionMenu";

const MOBILE_WIDTH = 426;

interface UserCardProps {
  user: User;
  setCurrSelectedUser: React.Dispatch<React.SetStateAction<User | null>>;
  currSelectedUser: User | null;
  setOpenAlert: React.Dispatch<React.SetStateAction<AlertType>>;
  setAlertMsg: React.Dispatch<React.SetStateAction<string>>;
  key: number;
}

const UserCard: FunctionComponent<UserCardProps> = ({
  user,
  setCurrSelectedUser,
  currSelectedUser,
  setOpenAlert,
  setAlertMsg,
}) => {
  const dimensions = useWindowDimensions();

  const handleClick = () => {
    if (!dimensions?.width || dimensions.width > MOBILE_WIDTH) return;
    setCurrSelectedUser(user);
  };

  return (
    <div className="w-full flex justify-center mt-2">
      <div
        onClick={handleClick}
        className="w-10/12 bg-white rounded-lg border border-gray-200 shadow-md p-1 sm:p-3"
      >
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
              src={user.avatar || "/assets/default-pp.webp"}
              alt=""
            />
          </div>

          {/* user info */}
          <div className="col-span-9 sm:col-span-6">
            <div className="grid grid-rows-2 sm:grid-rows-3 grid-flow-col gap-1">
              <div>
                <Link to={`/users/profile/${user.id}`}>
                  <span className="font-medium">{user?.username}</span>{" "}
                  <span className="text-xs font-light">{user?.intraName}</span>
                </Link>
              </div>

              <div>
                {user.status === "LOGGEDIN" ? (
                  <OnlineBadge />
                ) : user?.status === "OFFLINE" ? (
                  <OfflineBadge />
                ) : user?.status === "INGAME" ? (
                  <IngameBadge />
                ) : null}
              </div>

              <div className="hidden sm:block">Level {user.level}</div>

              <div className="text-xs sm:text-sm">Games played : 123</div>
            </div>
          </div>

          {/* Menu button for pc */}
          <div className="hidden sm:flex col-span-3 h-full justify-end items-end">
            <div
              className={`flex justify-end ${
                user.id == currSelectedUser?.id ? "relative" : ""
              }`}
            >
              <button
                onClick={() => {
                  setCurrSelectedUser(user);
                }}
              >
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  className="menu-toggle h-6 w-6"
                  fill="none"
                  viewBox="0 0 24 24"
                  stroke="currentColor"
                  strokeWidth={2}
                >
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    d="M12 5v.01M12 12v.01M12 19v.01M12 6a1 1 0 110-2 1 1 0 010 2zm0 7a1 1 0 110-2 1 1 0 010 2zm0 7a1 1 0 110-2 1 1 0 010 2z"
                  />
                </svg>
              </button>
              {user.id == currSelectedUser?.id ? (
                <div className=" top-6 action-menu absolute">
                  <ActionMenu
                    user={user}
                    setCurrUser={setCurrSelectedUser}
                    setAlertMsg={setAlertMsg}
                    setOpenAlert={setOpenAlert}
                  />
                </div>
              ) : null}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default UserCard;
