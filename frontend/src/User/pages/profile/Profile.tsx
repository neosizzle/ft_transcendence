import React, { FunctionComponent, useEffect, useState } from "react";
import { useAuth, User } from "../../../context/authContext";
import LevelBar from "../../common/LevelBar";
import MatchHistoryTable from "../../common/MatchHistoryTable";
import moment from "moment";
import { useLocation, useNavigate } from "react-router-dom";
import { auth_net_get } from "../../../utils";
import { API_ROOT, ERR, INCOMING_STATUS_UPDATE } from "../../../constants";
import { IngameBadge, OfflineBadge, OnlineBadge } from "../../common/Badges";
import { BaseWSResponse } from "../../../Chat/classes";

const userEndpoint = `${API_ROOT}/users?page=1&pageSize=1`;

const VerifiedBadge = () => {
  const [showToolTip, setShowToolTip] = useState<boolean>(false);
  return (
    <div className="inline relative">
      {showToolTip ? (
        <div
          className={`absolute left-7 top-0 text-white bg-black px-3 py-1 text-sm font-light rounded`}
        >
          Verified
        </div>
      ) : null}
      <span
        onMouseEnter={() => setShowToolTip(true)}
        onMouseLeave={() => setShowToolTip(false)}
        className="inline-flex items-center p-1 mr-2 text-sm font-semibold text-blue-800 bg-blue-100 rounded-full dark:bg-blue-200 dark:text-blue-800"
      >
        <svg
          className="w-3 h-3"
          fill="currentColor"
          viewBox="0 0 20 20"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            fillRule="evenodd"
            d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
            clipRule="evenodd"
          ></path>
        </svg>
      </span>
    </div>
  );
};

const Profile: FunctionComponent = () => {
  const auth = useAuth();
  const [levelPercent, setlevelPercent] = useState<number>(0);
  const [user, setUser] = useState<User | null>(null);
  const location = useLocation();
  const navigate = useNavigate();

  // update user status
  const handleUserUpdate = (data: User) => {
    if (data.id === user?.id) setUser(data);
  };

  const handleErr = (data: BaseWSResponse) => {
    alert(data.message);
  };

  useEffect(() => {
    if (!auth?.userSocket) return;

    auth.userSocket.on(INCOMING_STATUS_UPDATE, handleUserUpdate);
    auth.userSocket.on(ERR, handleErr);
    return () => {
      auth?.userSocket?.off(INCOMING_STATUS_UPDATE, handleUserUpdate);
      auth?.userSocket?.off(ERR, handleErr);
    };
  }, [auth]);

  useEffect(() => {
    const id = location.pathname.substring(
      location.pathname.lastIndexOf("/") + 1
    );

    if (id === "me" && auth?.user) setUser(auth?.user);
    else {
      //else, get user by id
      auth_net_get(`${userEndpoint}&filterBy=${id}&filterOn=id`).then(
        (data) => {
          //check error
          if (data.error) {
            if (data.error === "Forbidden") return navigate("/logout");

            console.error(data.error);
          }

          //set user data
          setUser(data.data[0]);
        }
      );
    }
  }, [location]);

  // change level percent when user changes
  useEffect(() => {
    if (!user) return;
    setlevelPercent(
      user?.level
        ? parseInt(
            ((user?.level - Math.floor(user?.level)) * 100).toFixed(1),
            10
          )
        : 0
    );
  }, [user]);

  return typeof auth === "string" ? null : user?.id ? (
    <div className="">
      <div
        className="
				grid
				gap-4
				grid-rows-3
				sm:grid-cols-3
				sm:grid-rows-none
				"
      >
        <div
          className="
					flex
					justify-center
					"
        >
          <img
            className="
						inline-block
						h-20
						w-20
						sm:h-28
						sm:w-28
						rounded-full
						ring-1
						ring-white
						mt-3
						mb-1
						"
            src={user?.avatar || "/assets/default-pp.webp"}
            alt=""
          />
        </div>

        <div
          className="
					flex
					justify-center
					sm:block
					sm:p-5
					"
        >
          <div>
            <span
              className="
							font-semibold
							text-lg
							block
							"
            >
              {" "}
              {user?.username || user?.intraName}
              <span
                className="
								font-normal
								text-sm
								"
              >
                {" "}
                {user?.intraName}{" "}
              </span>
              {user?.email ? <VerifiedBadge /> : null}
            </span>
            <span
              className="
							font-normal
							text-base
							block
							"
            >
              {moment(user?.createdAt).format("DD-MM-YYYY")}
            </span>
            {user?.status === "LOGGEDIN" ? (
              <OnlineBadge />
            ) : user?.status === "OFFLINE" ? (
              <OfflineBadge />
            ) : user?.status === "INGAME" ? (
              <IngameBadge />
            ) : null}
          </div>
        </div>

        <div
          className="
					sm:p-5
					flex
					justify-center
					sm:block
					"
        >
          <div
            className="
						h-full
						w-full
						rounded
						"
          >
            <div className=" grid sm:grid-cols-3 grid-cols-2 gap-4">
              <div className="sm:h-full sm:w-full rounded bg-slate-300 m-2 text-center">
                <div className="mx-auto">Rank</div>
                <div className="font-bold">{user.ranking}</div>
              </div>

              <div className="sm:h-full sm:w-full rounded bg-slate-300 m-2 text-center">
                <div>W / L</div>
                <div className="font-bold">
                  {user.losses
                    ? (user.wins / user.losses).toFixed(1)
                    : user.wins}
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      <div>
        <div className="flex justify-center">
          <div className="w-5/6 flex justify-between">
            <div
              className="
							inline-block
							"
            >
              Level{" "}
              {typeof user?.level === "number"
                ? Math.floor(user?.level)
                : "wut"}
            </div>
            <div
              className="
							inline-block
							ml-1
							"
            >
              {levelPercent}/100
            </div>
          </div>
        </div>
        <LevelBar percent={levelPercent} />
      </div>

      <MatchHistoryTable user={user} />
    </div>
  ) : (
    <div>user not found</div>
  );
};

export default Profile;
