import React, { FunctionComponent, useEffect, useState } from "react";
import { NavLink, useNavigate } from "react-router-dom";
import {
  API_ROOT,
  NOT_FRIENDS,
  OUTGOING_LEAVE,
  PENDING_FRIENDS,
} from "../../constants";
import { useAuth, User } from "../../context/authContext";
import { useChat } from "../../context/chatContext";
import { auth_net_delete, auth_net_get, auth_net_post } from "../../utils";
import { Member, Message } from "../classes";

const blocksEndpoint = `${API_ROOT}/blocks`;
const friendsEndpoint = `${API_ROOT}/friends`;
const chatEndpoint = `${API_ROOT}/chat`;

interface ChatAreaProps {
  members: Member[] | null;
  memberUsers: User[] | null;
  otherDmUser: User | null;
  isUserBlockedByYou: boolean;
  setIsUserBlockedByYou: React.Dispatch<React.SetStateAction<boolean>>;
  userFriendShipState: number;
  setUserFriendshipState: React.Dispatch<React.SetStateAction<number>>;
}

const ChatArea: FunctionComponent<ChatAreaProps> = ({
  members,
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
        roomId: chat?.activeRoom?.id,
        message: currentMessage,
      };
      auth?.chatSocket?.emit("message", JSON.stringify(dto));
      setCurrentMessage("");
    }
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setCurrentMessage(event.target.value);
  };

  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    if (e.currentTarget.scrollTop === 0) {
      auth_net_get(
        `${chatEndpoint}?page=2&pageSize=15&filterOn=roomId&filterBy=${chat?.activeRoom?.id}&sortBy=Descending&sortOn=createdAt`
      ).then((data) => {
        // token expired
        if (data.error && data.error == "Forbidden") return navigate("/logout");

        const chats = data.data;
        const msgsArr: Message[] = [];
        chats.forEach((chat: Message) => {
          msgsArr.unshift(chat);
        });
        chat?.setActiveRoomMessages(msgsArr);
      });
      const test = document.getElementById("dummy");
      test?.scrollIntoView();
    }
    console.log(e.currentTarget.scrollTop);
  }

  useEffect(() => {
    const test = document.getElementById("dummy");
    test?.scrollIntoView();
  }, [chat?.activeRoomMessages]);

  return (
    <div className="">
      <div className="flex flex-row">
        {chat?.activeRoom?.type === "DM" && memberUsers != null && memberUsers[1] != null && memberUsers[0] != null ? (
          auth?.user?.id === memberUsers[0].id ? (
            <p className="text-3xl">{memberUsers[1].username} </p>
          ) : (
            <p className="text-3xl ">{memberUsers[0].username} </p>
          )
        ) : (
          <p className="text-3xl ">
            {chat?.activeRoom?.roomName} - {chat?.activeRoom?.id}
          </p>
        )}
      </div>
      <div className="flex flex-row justify-around">
        {/* Profile btn */}
        {chat?.activeRoom?.type === "DM" ? (
          <NavLink
            to={`/users/profile/${otherDmUser?.id}`}
            className="block rounded px-4 py-2 bg-gray-400"
          >
            View Profile
          </NavLink>
        ) : null}

        {/* Spectate btn */}
        {chat?.activeRoom?.type === "DM" ? (
          <p className="block rounded px-4 py-2 bg-gray-400">Invite to game</p>
        ) : null}

        {/* Unfriend Btn */}
        {chat?.activeRoom?.type === "DM" ? (
          <button
            className={`block rounded px-4 py-2 bg-gray-400 ${userFriendShipState === PENDING_FRIENDS ? "disabled" : ""
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
            className="block rounded px-4 py-2 bg-gray-400"
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

        {/* Leave room Btn */}
        {chat?.activeRoom?.type === "GC" ? (
          <button
            onClick={() => {
              // get member id
              const memberId = members?.find(
                (member) => member.userId === auth?.user?.id
              )?.id;
              if (!memberId) return;

              // emit leave room dto
              auth?.chatSocket?.emit(OUTGOING_LEAVE, memberId.toString());
            }}
            className="block rounded px-4 py-2 bg-gray-400"
          >
            Leave
          </button>
        ) : null}
      </div>
      <div className="">
        <div className="h-96 overflow-scroll" onScroll={handleScroll}>
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
          <div id="dummy"></div>
        </div>
      </div>
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
