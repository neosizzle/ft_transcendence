import React, { FunctionComponent, useEffect, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import { API_ROOT } from "../../../../constants";
import { User } from "../../../../context/authContext";
import { auth_net_patch } from "../../../../utils";
import { FriendShip } from "../Friends";
import { IngameBadge, OfflineBadge, OnlineBadge } from "../../../common/Badges";

interface PendingCardProps {
  friendship?: FriendShip;
  currUser?: User | null;
  key?: number;
  setPendingUserAction: React.Dispatch<React.SetStateAction<User | null>>;
}

const friendShipEndpoint = `${API_ROOT}/friends`;

const PendingCard: FunctionComponent<PendingCardProps> = ({
  friendship,
  currUser,
  setPendingUserAction,
}) => {
  const [userToDisplay, setUserToDisplay] = useState<User | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const navigate = useNavigate();

  useEffect(() => {
    if (friendship?.userId === currUser?.id)
      setUserToDisplay(friendship?.friend || null);
    else setUserToDisplay(friendship?.user || null);
  }, []);

  const handleAction = async (reqStatus: string) => {
    setLoading(true);
    auth_net_patch(`${friendShipEndpoint}/${friendship?.id}`, {
      friendId: friendship?.userId,
      reqStatus,
    }).then((data) => {
      setLoading(false);
      if (data.error && data.error == "Forbidden") return navigate("/logout");
      setPendingUserAction(data.user);
    });
  };

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

            <div className="hidden sm:block">Level {typeof userToDisplay?.level === "number"
                ? Math.floor(userToDisplay?.level)
                : "wut"}</div>
          </div>

          {/* Action buttons */}
          <div className="col-span-3 h-full flex justify-end items-end">
            <button
              disabled={loading}
              onClick={() => handleAction("APPROVED")}
              type="button"
              className="text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-full text-sm p-2 text-center inline-flex items-center mr-2"
            >
              {loading ? (
                <svg
                  aria-hidden="true"
                  className="h-3 w-3 sm:h-4 sm:w-4 text-gray-200 animate-spin fill-green-600"
                  viewBox="0 0 100 101"
                  fill="none"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <path
                    d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
                    fill="currentColor"
                  />
                  <path
                    d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
                    fill="currentFill"
                  />
                </svg>
              ) : (
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
                    d="M5 13l4 4L19 7"
                  />
                </svg>
              )}
            </button>

            <button
              disabled={loading}
              onClick={() => handleAction("REJECTED")}
              type="button"
              className="text-white bg-red-700 hover:bg-red-800 focus:ring-4 focus:outline-none focus:ring-red-300 font-medium rounded-full text-sm p-2 text-center inline-flex items-center mr-2"
            >
              {loading ? (
                <svg
                  aria-hidden="true"
                  className="h-3 w-3 sm:h-4 sm:w-4 text-gray-200 animate-spin fill-red-600"
                  viewBox="0 0 100 101"
                  fill="none"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <path
                    d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
                    fill="currentColor"
                  />
                  <path
                    d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
                    fill="currentFill"
                  />
                </svg>
              ) : (
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
                    d="M6 18L18 6M6 6l12 12"
                  />
                </svg>
              )}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default PendingCard;
