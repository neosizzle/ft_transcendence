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

interface ActionMenuProps {
  user: User;
  setCurrUser: React.Dispatch<React.SetStateAction<User | null>>;
  setOpenAlert: React.Dispatch<React.SetStateAction<AlertType>>;
  setAlertMsg: React.Dispatch<React.SetStateAction<string>>;
}

const ActionMenu: FunctionComponent<ActionMenuProps> = ({
  user,
  setCurrUser,
  setOpenAlert,
  setAlertMsg,
}) => {
  const [loading, setLoading] = useState<boolean>(false); // loading state
  const auth = useAuth();
  const navigate = useNavigate();

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
    <div className="z-10 py-2 drop-shadow-md bg-white rounded border border-gray-200 w-[30rem]">
      {/* View profile */}
      <button
        className="block text-left hover:bg-gray-300 py-3 w-full px-4"
        disabled={loading}
        onClick={viewProfile}
      >
        {" "}
        View profile{" "}
      </button>

      {auth?.user?.id !== user.id ? (
        <div>
          {/* Add friend */}
          <button
            className="block text-left hover:bg-gray-300 py-3 w-full px-4"
            disabled={loading}
            onClick={addFriend}
          >
            {" "}
            Add friend{" "}
          </button>
          {/* Message user */}
          <button
            className="block text-left hover:bg-gray-300 py-3 w-full px-4"
            disabled={loading}
            onClick={messageUser}
          >
            {" "}
            Message{" "}
          </button>
          {/* Block User */}
          <button
            className="block text-left hover:bg-gray-300 py-3 w-full px-4"
            disabled={loading}
            onClick={blockUser}
          >
            {" "}
            Block / Unblock{" "}
          </button>
        </div>
      ) : null}
    </div>
  );
};

export default ActionMenu;
