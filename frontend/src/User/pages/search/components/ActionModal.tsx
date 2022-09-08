import React, { FunctionComponent, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Room } from "../../../../Chat/classes";
import { AlertType } from "../../../../commonComponents/Alert";
import { API_ROOT } from "../../../../constants";
import { useAuth, User } from "../../../../context/authContext";
import { auth_net_delete, auth_net_post } from "../../../../utils";
import { createNewDm, getCommonDmRoom } from "../../../utils";

const friendsEndpoint = `${API_ROOT}/friends`;
const blocksEndpoint = `${API_ROOT}/blocks`;

interface ActionModalProps {
  user: User;
  setOpenAlert: React.Dispatch<React.SetStateAction<AlertType>>;
  setAlertMsg: React.Dispatch<React.SetStateAction<string>>;
  setCurrUser: React.Dispatch<React.SetStateAction<User | null>>;
}

const ActionModal: FunctionComponent<ActionModalProps> = ({
  user,
  setAlertMsg,
  setOpenAlert,
  setCurrUser,
}) => {
  const navigate = useNavigate();
  const auth = useAuth();
  const [loading, setLoading] = useState<boolean>(false);

  const viewProfile = () => {
    navigate(`/users/profile/${user.id}`);
  };

  const messageUser = async () => {
    if (!auth || !auth.user) return;

    // try to get common dm room
    let commonDmRoom: Room | undefined = await getCommonDmRoom(
      auth,
      auth?.user,
      user
    );

    if (!commonDmRoom) {
      const newRoomData = await createNewDm(auth, user);
      if (newRoomData.id) commonDmRoom = newRoomData;
    }

    if (!commonDmRoom) {
      setOpenAlert({ type: "error", isOpen: true });
      setAlertMsg(`Cant message ${user.username}`);
    } else navigate(`/chat?room=${commonDmRoom?.id}`);
    setCurrUser(null);
  };

  const addFriend = () => {
    setLoading(true);
    auth_net_post(friendsEndpoint, { intraName: user.intraName }).then(
      (data) => {
        setLoading(false);
        if (data.error && data.error === "Forbidden") navigate("/logout");
        else if (data.error) {
          setOpenAlert({ type: "error", isOpen: true });
          setAlertMsg(data.message);
        } else {
          setOpenAlert({ type: "success", isOpen: true });
          setAlertMsg(`Request sent to ${user.username}`);
        }
      }
    );

    setCurrUser(null);
  };

  const blockUser = () => {
    setLoading(true);
    auth_net_post(blocksEndpoint, { intraName: user.intraName }).then(
      async (data) => {
        setLoading(false);
        if (data.error && data.error === "Forbidden") navigate("/logout");
        else if (data.error && data.message === "Already blocked person") {
          const unblockRes = await auth_net_delete(
            `${blocksEndpoint}/${user.id}`
          );
          if (unblockRes.error && unblockRes.error === "Forbidden")
            navigate("/logout");
          setOpenAlert({ type: "success", isOpen: true });
          setAlertMsg(`Unblocked ${user.username}`);
        } else if (data.error) {
          setOpenAlert({ type: "error", isOpen: true });
          setAlertMsg(data.message);
        } else {
          setOpenAlert({ type: "success", isOpen: true });
          setAlertMsg(`Blocked ${user.username}`);
        }
      }
    );

    setCurrUser(null);
  };


  return (
    <div className="fixed top-0 left-0 right-0 z-50 h-full overflow-x-hidden overflow-y-auto md:inset-0  bg-gray-300/50">
      <div className="relative w-full p-4 flex justify-center item-center">
        <div className="action-menu bg-white rounded-lg shadow dark:bg-gray-700">
          <button
            onClick={() => {
              setCurrUser(null);
            }}
            type="button"
            className="absolute top-3 right-2.5 text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center dark:hover:bg-gray-800 dark:hover:text-white"
            data-modal-toggle="popup-modal"
          >
            <svg
              className="w-5 h-5"
              fill="currentColor"
              viewBox="0 0 20 20"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                fillRule="evenodd"
                d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                clipRule="evenodd"
              ></path>
            </svg>
          </button>
          <div className="p-6 mt-4">
            {/* View profile */}
            <button
              className="block text-left py-3 w-full px-2"
              disabled={loading}
              onClick={viewProfile}
            >
              {" "}
              View {user.username}&apos;s profile{" "}
            </button>

            {auth?.user?.id !== user.id ? (
              <div>
                {/* Add friend */}
                <button
                  className="block text-left py-3 w-full px-2"
                  disabled={loading}
                  onClick={addFriend}
                >
                  {" "}
                  Add {user.username} as friend{" "}
                </button>
                {/* Message user */}
                <button
                  className="block text-left py-3 w-full px-2"
                  disabled={loading}
                  onClick={messageUser}
                >
                  {" "}
                  Message {user.username}{" "}
                </button>
                {/* Block User */}
                <button
                  className="block text-left py-3 w-full px-2"
                  disabled={loading}
                  onClick={blockUser}
                >
                  {" "}
                  Block / Unblock {user.username}{" "}
                </button>
              </div>
            ) : null}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ActionModal;
