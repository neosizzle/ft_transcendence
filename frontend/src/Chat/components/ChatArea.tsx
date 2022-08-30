import React, { FunctionComponent, useState } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import { API_ROOT, NOT_FRIENDS, PENDING_FRIENDS } from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { auth_net_delete, auth_net_post } from "../../utils";

const blocksEndpoint = `${API_ROOT}/blocks`;
const friendsEndpoint = `${API_ROOT}/friends`;

interface ChatAreaProps {
  memberUsers: User[] | null;
  otherDmUser: User | null;
  isUserBlockedByYou: boolean;
  setIsUserBlockedByYou: React.Dispatch<React.SetStateAction<boolean>>;
  userFriendShipState: number;
  setUserFriendshipState: React.Dispatch<React.SetStateAction<number>>;
}

const ChatArea: FunctionComponent<ChatAreaProps> = ({
  memberUsers,
  otherDmUser,
  isUserBlockedByYou,
  setIsUserBlockedByYou,
  userFriendShipState,
  setUserFriendshipState,
}) => {
  const chat = useChat();
  const auth = useAuth();
  const navigate = useNavigate();

  const [currentMessage, setCurrentMessage] = useState("");

  const sendMessage = async (event: React.FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    if (currentMessage !== "") {
      const dto = {
        userId: auth?.user?.id,
        roomId: 1,
        message: currentMessage,
      };
      auth?.chatSocket?.emit("message", JSON.stringify(dto));
      setCurrentMessage("");
    }
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setCurrentMessage(event.target.value);
  };

  return (
    <div className="h-96">
      <div className="flex flex-row">
        {chat?.activeRoom?.type === "DM" && memberUsers != null ? (
          auth?.user?.id === memberUsers[0].id ? (
            <p className="text-3xl border-2">{memberUsers[1].username} </p>
          ) : (
            <p className="text-3xl border-2">{memberUsers[0].username} </p>
          )
        ) : (
          <p className="text-3xl border-2">{chat?.activeRoom?.roomName}</p>
        )}
        <NavLink to="/users/profile/1" className="text-3xl border-2">
          View Profile
        </NavLink>
        <p className="text-3xl border-2">Spectate</p>

        {/* Unfriend Btn */}
        {chat?.activeRoom?.type === "DM" ? (
          <button
            className={`text-3xl border-2 ${
              userFriendShipState === PENDING_FRIENDS ? "disabled" : ""
            }`}
            onClick={() => {
              // return if user req is pending
              if (!otherDmUser || userFriendShipState === PENDING_FRIENDS)
                return;

              // if user is not friend, add friend
              if (userFriendShipState === NOT_FRIENDS)
                auth_net_post(`${friendsEndpoint}`, {
                  id: otherDmUser.id,
                }).then((data) => {
                  if (data.error && data.error == "Forbidden")
                    return navigate("/logout");

                  if (data.error) return alert("add friend error");
                });
              // else, delete friend
              else
                auth_net_delete(`${friendsEndpoint}/${otherDmUser.id}`).then(
                  (data) => {
                    if (data.error && data.error == "Forbidden")
                      return navigate("/logout");
                    if (data.error) return alert("remove friend error");
                    setUserFriendshipState(NOT_FRIENDS);
                  }
                );
            }}
          >
            {userFriendShipState === NOT_FRIENDS ? "Add friend" : "Unfriend"}
          </button>
        ) : null}

        {/* Block Btn */}
        {chat?.activeRoom?.type === "DM" ? (
          <button
            className="text-3xl border-2 block"
            onClick={() => {
              // return if active room is a GC
              if (
                chat?.activeRoom?.type === "GC" ||
                !memberUsers ||
                !otherDmUser
              )
                return;

              // user is already blocked by you, unblock is enabled
              if (isUserBlockedByYou)
                auth_net_delete(`${blocksEndpoint}/${otherDmUser.id}`).then(
                  (data) => {
                    if (data.error && data.error == "Forbidden")
                      return navigate("/logout");
                    if (data.error) return alert("unblock err");
                    setIsUserBlockedByYou(false);
                  }
                );
              // else, block user
              else
                auth_net_post(`${blocksEndpoint}`, {
                  id: otherDmUser.id,
                }).then((data) => {
                  if (data.error && data.error == "Forbidden")
                    return navigate("/logout");
                  if (data.error) return alert("block err");
                  setIsUserBlockedByYou(true);
                });
            }}
          >
            {isUserBlockedByYou ? "Unblock" : "Block"}
          </button>
        ) : null}
      </div>
      {chat?.activeRoomMessages?.map((message) => (
        <div key={message.id}>
          {message.userId === auth?.user?.id ? (
            <div>
              <div className="text-lg text-right">{message.message}</div>
              <div className="text-xs text-grey text-right">
                {new Date(message.createdAt).toLocaleString("en-gb", {
                  hour12: true,
                })}
              </div>
            </div>
          ) : (
            <div>
              <div className="text-lg">{message.message}</div>
              <div className="text-xs text-grey">
                {new Date(message.createdAt).toLocaleString("en-gb", {
                  hour12: true,
                })}
              </div>
            </div>
          )}
        </div>
      ))}
      <form onSubmit={sendMessage} className="Chat">
        <input
          type="text"
          className="w-fit mt-10 border-2"
          placeholder="Say something..."
          id="message"
          onChange={handleChange}
          value={currentMessage}
        />
      </form>
      {/* </div> */}
    </div>
  );
};

export default ChatArea;
