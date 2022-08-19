import React, { FunctionComponent, useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { API_ROOT } from "../../../constants";
import { useAuth, User } from "../../../context/authContext";
import { auth_net_get } from "../../../utils";
import FriendCard from "./components/FriendCard";
import SearchBar from "../../common/SearchBar";
import Pagination from "../../common/Pagination";
import PendingCard from "./components/PendingCard";
import UnfriendModal from "./components/UnfriendModal";
import CardLoader from "../../common/CardLoader";
import Alert, { AlertType } from "../../../commonComponents/Alert";

const friendsEndpoint = `${API_ROOT}/friends`;
const PAGE_SIZE = 5;

export interface FriendShip {
  id?: number;
  updatedAt?: string;
  createdAt?: string;
  friend?: User;
  friendId?: number;
  user?: User;
  userId?: number;
  reqStatus?: string;
}

const Friends: FunctionComponent = () => {
  const [friends, setFriends] = useState<FriendShip[] | null>(null); //friends list
  const [totalFriends, setTotalFriends] = useState<number>(-1); //total num of friends
  const [pendingFriends, setPendingFriends] = useState<FriendShip[] | null>(
    null
  ); //pending friends list
  const [totalPendingFriends, setTotalPendingFriends] = useState<number>(-1); //total num of pending friends
  const [currPage, setCurrPage] = useState<number>(1); //current page
  const [currPendingPage, setCurrPendingPage] = useState<number>(1); //current pending page
  const [currRemovingUser, setCurrRemovingUser] = useState<User | null>(null); //current user to unfriend
  const [openAlert, setOpenAlert] = useState<AlertType>({
    type: "",
    isOpen: false,
  }); //open alert box
  const [alertMsg, setAlertMsg] = useState<string>(""); //alert message
  const [friendsSearchInput, setFriendsSearchInput] = useState<string>(""); // friends search input
  const [pendingFriendsSearchInput, setPendingFriendsSearchInput] =
    useState<string>(""); // pending friends search input
  const [loading, setLoading] = useState<boolean>(false); // loading state for search operation
  const [pendingUserAction, setPendingUserAction] = useState<User | null>(null); //current user where a pending action is resolved
  const navigate = useNavigate();
  const auth = useAuth();

  // get data every page change
  useEffect(() => {
    if (!currRemovingUser) {
      //get active friends
      setLoading(true);
      setFriends(null);
      auth_net_get(
        `${friendsEndpoint}?&page=${currPage}&pageSize=${PAGE_SIZE}&filterOn=reqStatus,username&filterBy=APPROVED,${friendsSearchInput}`
      ).then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        setFriends(data.data);
        setTotalFriends(data.total_elements);
        setLoading(false);
      });
    }
  }, [currPage, currRemovingUser, friendsSearchInput, pendingUserAction]);

  // get data every pending friends page change
  useEffect(() => {
    // get pending friends
    if (!currRemovingUser) {
      setLoading(true);
      setFriends(null);
      auth_net_get(
        `${friendsEndpoint}?page=${currPage}&pageSize=${PAGE_SIZE}&filterOn=reqStatus,username&filterBy=PENDING,${pendingFriendsSearchInput}`
      ).then((data) => {
        if (data.error && data.error == "Forbidden") return navigate("/logout");
        setLoading(false);

        // only take pending incoming requests
        const filtered = data.data.filter(
          (e: FriendShip) => e.friendId === auth?.user?.id
        );

        setPendingFriends(filtered);
        setTotalPendingFriends(data.total_elements);
      });
    }
  }, [
    currPendingPage,
    currRemovingUser,
    pendingFriendsSearchInput,
    pendingUserAction,
  ]);

  return (
    <div>
      {/* Searchbar */}
      <SearchBar
        label="Search by username"
        setSearchInput={setFriendsSearchInput}
        loading={loading}
      />

      {!friends ? (
        <CardLoader />
      ) : friends.length < 1 ? (
        <div
          className="
				w-full
				flex
				justify-center
				"
        >
          <div className="w-10/12 mt-4 bg-white rounded-lg border border-gray-200 shadow-md p-10 flex flex-col justify-center items-center ">
            <img
              className="block h-20 sm:h-96 w-auto rounded"
              src="/assets/rock-sus.gif"
              alt="Workflow"
            />
            <div className="text-xl sm:text-2xl mt-6">You have no friends.</div>
          </div>
        </div>
      ) : (
        <div>
          {/* Friendslist */}
          {friends.map((elem) => {
            return (
              <FriendCard
                setCurrRemovingUser={setCurrRemovingUser}
                friendship={elem}
                currUser={auth?.user}
                key={elem.id}
              />
            );
          })}

          {/* Pagination */}
          {
            <Pagination
              setCurrPage={setCurrPage}
              currPage={currPage}
              totalElements={totalFriends}
              pageSize={PAGE_SIZE}
            />
          }

          {/* Unfriend confirmation */}
          {currRemovingUser ? (
            <UnfriendModal
              setAlertMsg={setAlertMsg}
              setOpenAlert={setOpenAlert}
              setCurrPage={setCurrPage}
              setCurrRemovingUser={setCurrRemovingUser}
              currRemovingUser={currRemovingUser}
            />
          ) : null}

          {/* pending Friendslist */}

          {/* Alert feedback */}
          {openAlert.isOpen ? (
            <Alert
              alert={openAlert}
              setOpenAlert={setOpenAlert}
              message={alertMsg}
            />
          ) : null}
        </div>
      )}

      <div className="mt-6 flex justify-center w-full">
        <p className="text-2xl block w-10/12">Pending requests</p>
      </div>
      {!pendingFriends ? (
        <CardLoader />
      ) : pendingFriends.length < 1 ? (
        <div
          className="
				w-full
				flex
				justify-center
				"
        >
          <div className="w-10/12 mb-40 sm:mb-0 mt-4 bg-white rounded-lg border border-gray-200 shadow-md p-10 flex flex-col justify-center items-center ">
            {new Date().getHours() < 13 ? (
              <img
                className="block h-20 sm:h-96 w-auto rounded"
                src="/assets/goodmorning.gif"
                alt="Workflow"
              />
            ) : (
              <img
                className="block h-20 sm:h-96 w-auto rounded"
                src="/assets/goodevening.gif"
                alt="Workflow"
              />
            )}
            <div className="text-xl sm:text-2xl mt-6">
              You have no pending requests.
            </div>
          </div>
        </div>
      ) : (
        <div className="pb-20 sm:pb-0">
          {/* pending Searchbar */}
          <SearchBar
            label="Search by username"
            setSearchInput={setPendingFriendsSearchInput}
            loading={loading}
          />

          {/* Friendslist */}
          {pendingFriends.map((elem) => {
            return (
              <PendingCard
                setPendingUserAction={setPendingUserAction}
                friendship={elem}
                currUser={auth?.user}
                key={elem.id}
              />
            );
          })}

          {/* Pagination */}
          {
            <Pagination
              setCurrPage={setCurrPendingPage}
              currPage={currPendingPage}
              totalElements={totalPendingFriends}
              pageSize={PAGE_SIZE}
            />
          }
        </div>
      )}
    </div>
  );
};

export default Friends;
